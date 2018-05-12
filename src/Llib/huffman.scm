(module hpack/huffman
   (library hoard srfi74)
   (import hpack/utils)
   (export (huffman-encode-string! str blob index bit-offset)
           (huffman-decode-string! blob index bit-offset encoded-length)))

(define +symbol->code-length+ '#((#x1ff8  13)
                                 (#x7fffd8  23)
                                 (#xfffffe2  28)
                                 (#xfffffe3  28)
                                 (#xfffffe4  28)
                                 (#xfffffe5  28)
                                 (#xfffffe6  28)
                                 (#xfffffe7  28)
                                 (#xfffffe8  28)
                                 (#xffffea  24)
                                 (#x3ffffffc  30)
                                 (#xfffffe9  28)
                                 (#xfffffea  28)
                                 (#x3ffffffd  30)
                                 (#xfffffeb  28)
                                 (#xfffffec  28)
                                 (#xfffffed  28)
                                 (#xfffffee  28)
                                 (#xfffffef  28)
                                 (#xffffff0  28)
                                 (#xffffff1  28)
                                 (#xffffff2  28)
                                 (#x3ffffffe  30)
                                 (#xffffff3  28)
                                 (#xffffff4  28)
                                 (#xffffff5  28)
                                 (#xffffff6  28)
                                 (#xffffff7  28)
                                 (#xffffff8  28)
                                 (#xffffff9  28)
                                 (#xffffffa  28)
                                 (#xffffffb  28)
                                 (#x14   6)
                                 (#x3f8  10)
                                 (#x3f9  10)
                                 (#xffa  12)
                                 (#x1ff9  13)
                                 (#x15   6)
                                 (#xf8   8)
                                 (#x7fa  11)
                                 (#x3fa  10)
                                 (#x3fb  10)
                                 (#xf9   8)
                                 (#x7fb  11)
                                 (#xfa   8)
                                 (#x16   6)
                                 (#x17   6)
                                 (#x18   6)
                                 (#x0   5)
                                 (#x1   5)
                                 (#x2   5)
                                 (#x19   6)
                                 (#x1a   6)
                                 (#x1b   6)
                                 (#x1c   6)
                                 (#x1d   6)
                                 (#x1e   6)
                                 (#x1f   6)
                                 (#x5c   7)
                                 (#xfb   8)
                                 (#x7ffc  15)
                                 (#x20   6)
                                 (#xffb  12)
                                 (#x3fc  10)
                                 (#x1ffa  13)
                                 (#x21   6)
                                 (#x5d   7)
                                 (#x5e   7)
                                 (#x5f   7)
                                 (#x60   7)
                                 (#x61   7)
                                 (#x62   7)
                                 (#x63   7)
                                 (#x64   7)
                                 (#x65   7)
                                 (#x66   7)
                                 (#x67   7)
                                 (#x68   7)
                                 (#x69   7)
                                 (#x6a   7)
                                 (#x6b   7)
                                 (#x6c   7)
                                 (#x6d   7)
                                 (#x6e   7)
                                 (#x6f   7)
                                 (#x70   7)
                                 (#x71   7)
                                 (#x72   7)
                                 (#xfc   8)
                                 (#x73   7)
                                 (#xfd   8)
                                 (#x1ffb  13)
                                 (#x7fff0  19)
                                 (#x1ffc  13)
                                 (#x3ffc  14)
                                 (#x22   6)
                                 (#x7ffd  15)
                                 (#x3   5)
                                 (#x23   6)
                                 (#x4   5)
                                 (#x24   6)
                                 (#x5   5)
                                 (#x25   6)
                                 (#x26   6)
                                 (#x27   6)
                                 (#x6   5)
                                 (#x74   7)
                                 (#x75   7)
                                 (#x28   6)
                                 (#x29   6)
                                 (#x2a   6)
                                 (#x7   5)
                                 (#x2b   6)
                                 (#x76   7)
                                 (#x2c   6)
                                 (#x8   5)
                                 (#x9   5)
                                 (#x2d   6)
                                 (#x77   7)
                                 (#x78   7)
                                 (#x79   7)
                                 (#x7a   7)
                                 (#x7b   7)
                                 (#x7ffe  15)
                                 (#x7fc  11)
                                 (#x3ffd  14)
                                 (#x1ffd  13)
                                 (#xffffffc  28)
                                 (#xfffe6  20)
                                 (#x3fffd2  22)
                                 (#xfffe7  20)
                                 (#xfffe8  20)
                                 (#x3fffd3  22)
                                 (#x3fffd4  22)
                                 (#x3fffd5  22)
                                 (#x7fffd9  23)
                                 (#x3fffd6  22)
                                 (#x7fffda  23)
                                 (#x7fffdb  23)
                                 (#x7fffdc  23)
                                 (#x7fffdd  23)
                                 (#x7fffde  23)
                                 (#xffffeb  24)
                                 (#x7fffdf  23)
                                 (#xffffec  24)
                                 (#xffffed  24)
                                 (#x3fffd7  22)
                                 (#x7fffe0  23)
                                 (#xffffee  24)
                                 (#x7fffe1  23)
                                 (#x7fffe2  23)
                                 (#x7fffe3  23)
                                 (#x7fffe4  23)
                                 (#x1fffdc  21)
                                 (#x3fffd8  22)
                                 (#x7fffe5  23)
                                 (#x3fffd9  22)
                                 (#x7fffe6  23)
                                 (#x7fffe7  23)
                                 (#xffffef  24)
                                 (#x3fffda  22)
                                 (#x1fffdd  21)
                                 (#xfffe9  20)
                                 (#x3fffdb  22)
                                 (#x3fffdc  22)
                                 (#x7fffe8  23)
                                 (#x7fffe9  23)
                                 (#x1fffde  21)
                                 (#x7fffea  23)
                                 (#x3fffdd  22)
                                 (#x3fffde  22)
                                 (#xfffff0  24)
                                 (#x1fffdf  21)
                                 (#x3fffdf  22)
                                 (#x7fffeb  23)
                                 (#x7fffec  23)
                                 (#x1fffe0  21)
                                 (#x1fffe1  21)
                                 (#x3fffe0  22)
                                 (#x1fffe2  21)
                                 (#x7fffed  23)
                                 (#x3fffe1  22)
                                 (#x7fffee  23)
                                 (#x7fffef  23)
                                 (#xfffea  20)
                                 (#x3fffe2  22)
                                 (#x3fffe3  22)
                                 (#x3fffe4  22)
                                 (#x7ffff0  23)
                                 (#x3fffe5  22)
                                 (#x3fffe6  22)
                                 (#x7ffff1  23)
                                 (#x3ffffe0  26)
                                 (#x3ffffe1  26)
                                 (#xfffeb  20)
                                 (#x7fff1  19)
                                 (#x3fffe7  22)
                                 (#x7ffff2  23)
                                 (#x3fffe8  22)
                                 (#x1ffffec  25)
                                 (#x3ffffe2  26)
                                 (#x3ffffe3  26)
                                 (#x3ffffe4  26)
                                 (#x7ffffde  27)
                                 (#x7ffffdf  27)
                                 (#x3ffffe5  26)
                                 (#xfffff1  24)
                                 (#x1ffffed  25)
                                 (#x7fff2  19)
                                 (#x1fffe3  21)
                                 (#x3ffffe6  26)
                                 (#x7ffffe0  27)
                                 (#x7ffffe1  27)
                                 (#x3ffffe7  26)
                                 (#x7ffffe2  27)
                                 (#xfffff2  24)
                                 (#x1fffe4  21)
                                 (#x1fffe5  21)
                                 (#x3ffffe8  26)
                                 (#x3ffffe9  26)
                                 (#xffffffd  28)
                                 (#x7ffffe3  27)
                                 (#x7ffffe4  27)
                                 (#x7ffffe5  27)
                                 (#xfffec  20)
                                 (#xfffff3  24)
                                 (#xfffed  20)
                                 (#x1fffe6  21)
                                 (#x3fffe9  22)
                                 (#x1fffe7  21)
                                 (#x1fffe8  21)
                                 (#x7ffff3  23)
                                 (#x3fffea  22)
                                 (#x3fffeb  22)
                                 (#x1ffffee  25)
                                 (#x1ffffef  25)
                                 (#xfffff4  24)
                                 (#xfffff5  24)
                                 (#x3ffffea  26)
                                 (#x7ffff4  23)
                                 (#x3ffffeb  26)
                                 (#x7ffffe6  27)
                                 (#x3ffffec  26)
                                 (#x3ffffed  26)
                                 (#x7ffffe7  27)
                                 (#x7ffffe8  27)
                                 (#x7ffffe9  27)
                                 (#x7ffffea  27)
                                 (#x7ffffeb  27)
                                 (#xffffffe  28)
                                 (#x7ffffec  27)
                                 (#x7ffffed  27)
                                 (#x7ffffee  27)
                                 (#x7ffffef  27)
                                 (#x7fffff0  27)
                                 (#x3ffffee  26)
                                 (#x3fffffff  30)))


(define (symbol->code-length c::char)
   (vector-ref +symbol->code-length+ (char->integer c)))

(define +default-string-length+ 32)

(define (huffman-encode-string! str blob index bit-offset)
   ;; Determine the true byte index and residual bit off-set.
   ;; In general, bit-offset can be greater than 8. 
   (let* ((extra-bytes (/fx bit-offset 8))
          (residual-bit-offset (modulofx bit-offset 8))
          (byte-index (+ index extra-bytes)))

      ;; loop over the chars in string encoding each in turn
      (do ((i 0 (+ i 1)))
          ((= i (string-length str)))

          ;; obtain the each character's huffman code and length
          (let* ((code+len (symbol->code-length (string-ref str i)))
                 (code (car code+len))
                 (len (cadr code+len)))

             ;; write the code for current char to blob at byte-index with
             ;; a bit offset of residual-bit-offset
             (blob-bits-set! blob byte-index residual-bit-offset len code)

             ;; update byte-index and residual-bit-offset give the code
             ;; we just wrote
             (if (> (+ residual-bit-offset len) 7)
                (begin (set! byte-index (+ byte-index (/fx (+ residual-bit-offset len) 8)))
                       (set! residual-bit-offset (modulofx (+ residual-bit-offset len) 8)))
                (set! residual-bit-offset (+ residual-bit-offset len)))))

      ;; if we finish encoding the string and we have remaining room in the last byte
      ;; write the remaing bits with the prefix of the eos code
      (when (> residual-bit-offset 0)
         (blob-bits-set! blob byte-index residual-bit-offset (- 8 residual-bit-offset)
            (bit-and #xff (- (bit-lsh 1 (- 8 residual-bit-offset)) 1)))
         (set! byte-index (+ byte-index 1)))

      ;; finally return the length of the encoded string
      (- byte-index (+ index extra-bytes))))


(define (huffman-decode-string! blob index bit-offset encoded-length)
   (let loop  ((i bit-offset)
               (str-i 0)
               (res (make-string +default-string-length+)))
      (receive (sym len) (decode-symbol +huffman-lut+ blob index i encoded-length)
               (if (= sym 256)
                   (string-shrink! res str-i)
                   (begin
                      (string-set! res str-i (integer->char sym))
                      (loop  (+ i len)
                         (+ str-i 1)
                         (if (= (+ str-i 1) (string-length res))
                             (string-grow res)
                             res)))))))


(define (decode-symbol lut blob index bit-offset encoded-length)
   (let loop ((level 0)
              (curr-lut lut)
              (byte (next-byte blob index bit-offset encoded-length)))
      (let ((res (hashtable-get curr-lut byte)))
         (cond ((pair? res)
                (values (car res)
                   (+ (* level 8) (cdr res))))
               ((hashtable? res)
                (loop (+ level 1)
                   res
                   (next-byte blob index (+ bit-offset 8) encoded-length)))
               (else
                (error "decode-symbol" "decode error" byte)))))) 


;;;; lookup table implementation
(define +huffman-lut+ (build-lut))

(define (build-lut)
   (let ((lut (create-hashtable :eqtest =fx)))
      (dictionary-enumerable-for-each
         (lambda (i p)
            (extend-lut! lut i (car p) (cadr p)))
         +symbol->code-length+)
      lut))

(define (extend-lut! lut sym code len)
   (if (<= len 8)
       (do ((i 0 (+ i 1)))
           ((= i (bit-lsh 1 (- 8 len))))
           (hashtable-put! lut (+ (bit-lsh code (- 8 len)) i) (cons sym len)))
       (let* ((code-prefix (bit-rsh code (- len 8)))
              (sublut (if (hashtable-contains? lut code-prefix)
                          (hashtable-get lut code-prefix)
                          (let ((new-lut (create-hashtable :eqtest =fx :size 8)))
                             (hashtable-put! lut code-prefix new-lut)
                             new-lut))))
          (extend-lut! sublut sym (bit-and code (- (bit-lsh 1 (- len 8)) 1))
             (- len 8)))))
