(module hpack/hpack
   (import hpack/dynamic-table)
   (import hpack/static-table)
   (import hpack/huffman)
   (import hpack/utils)
   (library srfi74 hoard)
   (export
      (make-hpack-context max-size::long)
      (hpack-context-dynamic-table context)
      (hpack-encode! context headers #!key (enable-huffman #t))
      (hpack-decode! context blob #!optional (length #unspecified)
         #!key (max-n-headers 65536))))
   
;;; hpack implementation
(define (make-hpack-context max-size::long)
   (make-dynamic-table max-size))
         
(define (hpack-context-dynamic-table context)
   context)

(define (hpack-decode! context blob #!optional (length #unspecified)
           #!key (max-n-headers 65536))
   (let ((encoded-length (if (not (eq? length #unspecified)) length
                             (blob-length blob))))
      (let loop ((byte-offset 0)
                 (count 0)
                 (res '()))
         (when (> count max-n-headers)
            (error "hpack-decode"
               (format "exceeded maximum number(~a) of decoded headers"
                  max-n-headers) count))
         (if (< byte-offset encoded-length)
             (if (dynamic-table-size-update? blob byte-offset 0)
                 (receive (new-size new-byte-offset)
                    (decode-dynamic-table-update
                       (hpack-context-dynamic-table context)
                       blob byte-offset 3)
                    (loop new-byte-offset
                       count
                       res))
                 (receive (header new-byte-offset)
                    (decode-header-field (hpack-context-dynamic-table context)
                       blob byte-offset 0) 
                    (loop new-byte-offset
                       (+ count 1)
                       (cons header res))))
             (reverse! res)))))

(define +indexed-header-field-bit-offset+ 1)
(define +literal-header-field-w/-indexing-bit-offset+ 2)
(define +literal-header-field-w/out-indexing-bit-offset+ 4)

(define (decode-header-field table blob byte-offset bit-offset)
   (cond ((indexed-header-field? blob byte-offset bit-offset)
             (decode-indexed-header-field table blob byte-offset
                (+ bit-offset +indexed-header-field-bit-offset+)))
            ((literal-header-field-w/-indexing? blob byte-offset bit-offset)
             (receive (nv new-byte-offset)
                (decode-literal-header-field table blob byte-offset
                   (+ bit-offset +literal-header-field-w/-indexing-bit-offset+))
                (dynamic-table-extend! table (car nv) (cadr nv))
                (values nv new-byte-offset)))
            ((or (literal-header-field-never-indexed? blob byte-offset bit-offset)
                 (literal-header-field-w/out-indexing? blob byte-offset bit-offset))
             (decode-literal-header-field table blob byte-offset
                (+ bit-offset +literal-header-field-w/out-indexing-bit-offset+)))
            (else
             (error "decode-header-field" "unknown header field type"
                #unspecified))))

(define (decode-dynamic-table-update table blob byte-offset bit-offset)
   (receive (new-size new-byte-offset)
      (decode-integer blob byte-offset bit-offset)
      (if (<= new-size (dynamic-table-max-size table))
          (dynamic-table-resize! table new-size)
          (error "decode-dynamic-table-update" "invalid new table size"
             new-size))
      (values new-size new-byte-offset)))

;; tables-get assumes 1-based indexing not 0-based
(define (tables-get table index)
   (cond ((<= index (static-table-length))
          (static-table-ref index))
         ((<= (- index (static-table-length)) (dynamic-table-length table))
          (dynamic-table-ref table (- index (static-table-length))))
         (else
          (error "tables-get" "invalid table index" index))))

(define (tables-search table header)
   (let ((static-res (static-table-search header)))
      (if (and static-res (eq? (car static-res) 'full))
          static-res
          (let ((dynamic-res (dynamic-table-search table header)))
             (cond ((and dynamic-res (eq? (car dynamic-res) 'full))
                    dynamic-res)
                   ((and dynamic-res (not static-res))
                    dynamic-res)
                   (else static-res))))))

(define (decode-indexed-header-field table blob byte-offset bit-offset)
   (receive (index new-byte-offset) (decode-integer blob byte-offset bit-offset)
            (if (= index 0)
                (error "decode-indexed-header-field" "invalid index value" index)
                (values (tables-get table index) new-byte-offset))))

(define (decode-literal-header-field table blob byte-offset bit-offset)
   (receive (index new-byte-offset) (decode-integer blob byte-offset bit-offset)
            (cond ((> index 0)
                   (let ((name (car (tables-get table index))))
                      (receive (value byte-offset-2)
                         (decode-hpack-string blob new-byte-offset 0)
                         (values (list name value)  byte-offset-2))))
                  (else
                   (receive (name byte-offset-3)
                      (decode-hpack-string blob new-byte-offset 0)
                      (receive (value byte-offset-4)
                         (decode-hpack-string blob byte-offset-3 0)
                         (values (list name value) byte-offset-4)))))))

(define (decode-hpack-string blob byte-offset bit-offset)
   (let ((huffman-encoded? (huffman-encoded-string? blob byte-offset
                              bit-offset)))
      (receive (length new-byte-offset)
         (decode-integer blob byte-offset (+ bit-offset 1))
         (if huffman-encoded?
             (values (huffman-decode-string! blob new-byte-offset 0 length)
                (+ new-byte-offset length))
             (values (blob-str-ref blob new-byte-offset length)
                     (+ new-byte-offset length))))))

(define (indexed-header-field? blob index bit-offset)
   (= (bit-and  (bit-lsh (blob-u8-ref blob index) bit-offset) #x80)
      #x80))

(define (literal-header-field-w/-indexing? blob index bit-offset)
   (= (bit-and (bit-lsh (blob-u8-ref blob index) bit-offset) #xc0)
      #x40))
(define (literal-header-field-w/out-indexing? blob index bit-offset)
   (= (bit-and (bit-lsh (blob-u8-ref blob index) bit-offset) #xf0)
      0))

(define (literal-header-field-never-indexed? blob index bit-offset)
   (= (bit-and (bit-lsh (blob-u8-ref blob index) bit-offset) #xf0)
      #x10))

(define (dynamic-table-size-update? blob byte-offset bit-offset)
   (= (bit-and (bit-lsh (blob-u8-ref blob byte-offset) bit-offset) #xe0)
      #x20))

(define (decode-integer blob index bit-offset)
   (let* ((2^n-1 (- (bit-lsh 1  (- 8 bit-offset)) 1))
          (i  (bit-and 2^n-1 (blob-u8-ref blob index))))
      (if (< i 2^n-1)
          (values i (+ index 1))
          (let loop ((m 0)
                     (offset 2)
                     (next-byte (blob-u8-ref blob (+ index 1)))
                     (res i))
             (let ((nr (+ res (* (bit-and next-byte #x7f) (bit-lsh 1 m)))))
                (if (not (= (bit-and next-byte #x80) #x80))
                    (values nr   (+ index offset))
                    (loop (+ m 7)
                       (+ offset 1)
                       (blob-u8-ref blob (+ index offset))
                       nr)))))))

(define (encode-integer! blob index bit-offset val)
   (let* ((bit-prefix (- 8 bit-offset))
          (2^n-1 (- (bit-lsh 1 bit-prefix) 1)))         
      (if (< val 2^n-1)
          (begin
             (blob-bits-set! blob index bit-offset bit-prefix val)
             (+ index 1))
          (begin
             (blob-bits-set! blob index bit-offset bit-prefix 2^n-1)
             (let loop ((curr (- val 2^n-1))
                        (index2 (+ index 1)))             
                (if  (> curr 128)
                   (begin (blob-bits-set! blob index2 0 8
                                    (+ (modulo curr 128) 128))
                          (loop (/ curr 128)
                             (+ index2 1))) 
                   (begin
                      (blob-bits-set! blob index2 0 8 curr)
                      (+ index2 1))))))))

(define (huffman-encoded-string? blob byte-offset bit-offset)
   (= 1 (blob-bit-ref blob byte-offset bit-offset)))


(define (header-indexing header)
   (if (> (length header) 2)
       (list-ref header 2)
       'indexed))

(define (encode-header! table blob offset huffman? header)
   (case (header-indexing header)
      ((indexed)
       (encode-header-w/-indexing! table blob offset huffman? header))
      ((non-indexed)
       (encode-literal-header-w/out-indexing! table blob offset huffman? header))
      ((never-indexed)
       (encode-literal-header-never-indexed! table blob offset huffman? header))
      (else
       (error "encode-header" "unknown header indexing type"
          (header-indexing header)))))

(define (encode-string! blob offset str #!optional (huffman? #t))
   (if huffman?
       ;; create a tempory blob for encoding the huffman compressed
       ;; value
       (let* ((str-blob (make-blob (string-length str)))
             ;; huffman encode the string
              (encoded-str-len (huffman-encode-string! str str-blob 0 0)))
          ;; set huffman bit to true
          (blob-bits-set! blob offset 0 1 1)
          ;;encode the encoded string length into blob
          (let ((offset2 (encode-integer! blob offset 1 encoded-str-len)))
             ;; copy the encoded string into our target blob
             (blob-copy! str-blob 0 blob offset2 encoded-str-len)
             (+ offset2 encoded-str-len)))
       (let ((str-len (string-length str)))
          ;; set huffman bit to false
          (blob-bits-set! blob offset 0 1 0)
          (let ((offset2 (encode-integer! blob offset 1 str-len)))
             (blob-str-set! blob offset2 str)
             (+ offset2 str-len)))))

(define (encode-header-w/-indexing! table blob offset huffman? header)
   (let ((search-res (tables-search table header)))
      (if search-res
          (case (car search-res)
             ((full)
              ;; set 1-bit value of 1 indicating a fully indexed 
              ;; header
              (blob-bits-set! blob offset 0 1 1)
              ;; encode name-value index
              (encode-integer! blob offset 1 (cadr search-res)))
             ((partial)
              ;; add header to dynamic table
              (dynamic-table-extend! table (car header) (cadr header))
              ;; set 2-bit value of 1 indicating a literal header 
              ;; field with indexing
              (blob-bits-set! blob offset 0 2 1)
              ;; encode name index
              (let ((offset2 (encode-integer! blob offset 2
                                (cadr search-res))))
                 (encode-string! blob offset2 (cadr header) huffman?)))
             (else
              (error "encode-header-w/indexing"
                 "unknown table search result type"
                 (car search-res))))
          ;; no index found for header or value
          (begin
             ;; add header to dynamic table
             (dynamic-table-extend! table (car header) (cadr header))
             ;; set 2-bit value of 1 indicating literal header 
             ;; field with indexing
             (blob-bits-set! blob offset 0 2 1)
             ;; encode name
             (let ((offset2 (encode-string! blob (+ offset 1)
                               (car header) huffman?)))
                ;; encode value
                (encode-string! blob offset2 (cadr header) huffman?))))))

(define (encode-literal-header-w/out-indexing! table blob offset huffman? header)   
   ;; zero first byte. this sets the value of the most significant 4-bits to 0
   ;; indicating a non-indexed literal header field
   (blob-u8-set! blob offset 0)
             
   (let ((search-res (tables-search table header)))
      ;; encode name
      (let ((offset2 (if search-res
                         ;; we have an indexed name
                         (let ((index (cadr search-res)))
                            (encode-integer! blob offset
                               +literal-header-field-w/out-indexing-bit-offset+
                               index))
                         ;; we have a literal name
                         (encode-string! blob (+ offset 1)
                            (car header) huffman?))))
         ;; encode value
         (encode-string! blob offset2 (cadr header) huffman?))))

(define (encode-literal-header-never-indexed! table blob offset huffman? header)   
   ;; set first byte to #x10. this sets the value of the most significant 4-bits
   ;; to 1 indicating a never-indexed literal header field
   (blob-u8-set! blob offset #x10)
          
   (let ((search-res (tables-search table header)))
      ;; encode name
      (let ((offset2 (if search-res
                         ;; we have an indexed name
                         (let ((index (cadr search-res)))
                            (encode-integer! blob offset
                               +literal-header-field-w/out-indexing-bit-offset+
                               index))
                         ;; we have a literal name
                         (encode-string! blob (+ offset 1)
                            (car header) huffman?))))
         ;; encode value
         (encode-string! blob offset2 (cadr header) huffman?))))

(define (headers-size headers)
   (let loop ((hdrs headers)
              (size 0))
      (if (pair? hdrs)
          (let* ((header (car hdrs))
                 (name (car header))
                 (value (cadr header)))
             (loop (cdr hdrs)
                (+ size (string-length name)
                   (string-length value))))
          size)))

(define (hpack-encode! context headers #!key (enable-huffman #t))
   ;; we assume a 33% reduction in size, if wrong the blob will be resized to
   ;; accomodate
   (let ((blob (make-blob (/fx (*fx (headers-size headers) 2) 3)))
         (table (hpack-context-dynamic-table context)))
      (let  loop ((hdrs headers)
                  (offset 0))
         (if (pair? hdrs)
             (loop (cdr hdrs)
                (encode-header! table blob offset enable-huffman (car hdrs)))
          (values offset blob)))))
   