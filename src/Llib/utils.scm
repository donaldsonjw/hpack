(module hpack/utils
   (library srfi74)
   (export (next-byte blob byte-index bit-offset encoded-length)
           (string-grow str)))

(define (next-byte blob byte-index bit-offset encoded-length)
   (let*  ((extra-bytes (/fx bit-offset 8))
           (residual-bits  (modulofx bit-offset 8))
           (target-index (+ byte-index extra-bytes)))
      (cond ((and (= residual-bits 0)
                  (< target-index  (+ byte-index encoded-length)))
             (blob-u8-ref blob target-index))
            ((and (> residual-bits 0)
                  (< target-index (+ byte-index encoded-length)))
             (if (< (+ target-index 1) (+ byte-index encoded-length))
                 (bit-or  (bit-and #xff (bit-lsh (blob-u8-ref blob target-index)
                                           residual-bits))
                    (bit-rsh (blob-u8-ref blob (+ target-index 1))
                       (- 8 residual-bits)))
                 (bit-or (bit-and #xff (bit-lsh (blob-u8-ref blob target-index) residual-bits))
                    (bit-rsh #xff (- 8 residual-bits)))))
            (else
             #xff))))

;; double the size of str retaining the original contents
(define (string-grow str)
   (let* ((olen (string-length str))
          (res (make-string (*fx 2 olen))))
      (blit-string! str 0 res 0 olen)
      res))

