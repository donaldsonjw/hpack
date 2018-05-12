(module hpack/dynamic-table
   (library hoard)
   (import hpack/static-table)
   (export
      (class %dynamic-table
         max-size
         size
         buffer)
      (make-dynamic-table::%dynamic-table #!optional (max-size 65536))
      (dynamic-table-length::long table::%dynamic-table)
      (dynamic-table-ref table::%dynamic-table index::long)
      (dynamic-table-extend! table::%dynamic-table header #!optional (val '()))
      (dynamic-table-size::long table::%dynamic-table)
      (dynamic-table-resize! table::%dynamic-table new-size::long)
      (dynamic-table-search table::%dynamic-table header)
      (dynamic-table-max-size::long table::%dynamic-table)))

(define +default-dynamic-table-capacity+ 32)

(define +dynamic-table-field-overhead+ 32)

(define (make-dynamic-table::%dynamic-table #!optional (max-size 65536))
   (instantiate::%dynamic-table (max-size max-size)
                               (size 0)
                               (buffer (make-ring-buffer
                                          :capacity +default-dynamic-table-capacity+))))

(define (dynamic-table-length::long table::%dynamic-table)
   (ring-buffer-length (-> table buffer)))

(define (dynamic-table-size::long table::%dynamic-table)
   (-> table size))

(define (dynamic-table-max-size::long table::%dynamic-table)
   (-> table max-size))

;; table indexing is 1-based not 0-based
(define (dynamic-table-ref table::%dynamic-table index::long)
   (ring-buffer-ref (-> table buffer)  (- index 1)))

(define (field-size header val)
   (+ +dynamic-table-field-overhead+
      (string-length header)
      (if (not (null? val)) (string-length val) 0)))

(define (dynamic-table-extend! table::%dynamic-table header #!optional (val '()))
   (let* ((add-size (field-size header val))
          (new-size (+ (-> table size) add-size)))
      (when (> new-size (-> table max-size))
         (dynamic-table-evict! table (- new-size (-> table max-size))))

      ;; if needed resize table to hold entries
      (when (= (ring-buffer-capacity (-> table buffer))
               (ring-buffer-length (-> table buffer)))
         (ring-buffer-grow! (-> table buffer)
            (* 2 (ring-buffer-capacity (-> table buffer)))))

      ;; add entry
      (let ((entry (list header val)))
         (ring-buffer-push-front! (-> table buffer) entry)
         (set! (-> table size) (+ (-> table size) add-size)))))

(define (dynamic-table-evict! table::%dynamic-table size::long)
   (let loop ((reduced-size 0))
      (if  (and (not (ring-buffer-empty? (-> table buffer)))
                (< reduced-size size))
           (let* ((next (ring-buffer-pop-back! (-> table buffer)))
                  (fsize (field-size (car next) (cadr next))))
              (set! (-> table size) (- (-> table size) fsize))
              (loop (+ reduced-size fsize))))))

(define (dynamic-table-resize! table::%dynamic-table new-size::long)
   (dynamic-table-evict! table (- (-> table size) new-size)))

(define (dynamic-table-search-old table::%dynamic-table header)
   (bind-exit (return)
      (do ((i 0 (+ i 1)))
          ((= i (ring-buffer-length (-> table buffer))) #f)
          (let ((e (ring-buffer-ref (-> table buffer) i)))
             (if (string=? (car header) (car e))
                 (return  (list (if (string=? (cadr header) (cadr e))
                                    'full
                                    'partial)
                             (+ i 1 (static-table-length)))))))))


(define (dynamic-table-search table::%dynamic-table header)
   (let ((candidates (do ((i 0 (+ i 1))
                          (res '()))
                         ((= i (ring-buffer-length (-> table buffer))) res)
                         (let ((e (ring-buffer-ref (-> table buffer) i)))
                            (when (string=? (car header) (car e))
                               (if (string=? (cadr header) (cadr e))
                                   (set! res (cons (list 'full
                                                      (+ i 1
                                                         (static-table-length)))
                                                res))
                                   (set! res (cons  (list 'partial
                                                       (+ i 1
                                                          (static-table-length)))
                                                res))))))))
      (let ((full-match (find (lambda (p) (eq? (car p) 'full)) candidates)))
         (or full-match
             (and (pair? candidates) (car candidates))))))
                                
                  

