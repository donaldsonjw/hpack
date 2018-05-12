(module testhpack
   (library btest hpack srfi74 hoard)
   (main main))


(define (print-blob blob)
   (do ((i 0 (+ i 1)))
       ((= i (blob-length blob)))
       (printf "~x " (blob-u8-ref blob i)))
   (printf "~%"))

(define (test-series-of-3-requests b1 b2 b3)
   (let* ((context (make-hpack-context 65536))
          (table (hpack-context-dynamic-table context)))
      (assert-equal? (hpack-decode! context b1) '((":method" "GET")
                                                  (":scheme" "http")
                                                  (":path" "/")
                                                  (":authority"
                                                     "www.example.com")))
      (assert= (dynamic-table-size table) 57)
      (assert-equal? (dynamic-table-ref table 1) '(":authority"
                                                   "www.example.com"))
      (assert-equal? (hpack-decode! context b2)
         '((":method" "GET")
           (":scheme" "http")
           (":path" "/")
           (":authority"
              "www.example.com")
           ("cache-control" "no-cache")))
      (assert= (dynamic-table-size table) 110)
      (assert-equal? (dynamic-table-ref table 1)
         '("cache-control"  "no-cache"))

      (assert-equal? (hpack-decode! context b3)
         '((":method" "GET")
           (":scheme" "https")
           (":path" "/index.html")
           (":authority" "www.example.com")
           ("custom-key" "custom-value")))
      (assert= (dynamic-table-size table) 164)
      (assert-equal? (dynamic-table-ref table 1) '("custom-key"
                                                   "custom-value"))
      (assert-equal? (dynamic-table-ref table 2)
         '("cache-control" "no-cache"))
      (assert-equal? (dynamic-table-ref table 3)
         '(":authority"
           "www.example.com"))))


(define (test-series-of-3-responses b1 b2 b3)
   (let* ((context (make-hpack-context 256))
          (table (hpack-context-dynamic-table context)))
      (assert-equal? (hpack-decode! context b1)
         '((":status" "302")
           ("cache-control" "private")
           ("date" "Mon, 21 Oct 2013 20:13:21 GMT")
           ("location" "https://www.example.com")))
      (assert= (dynamic-table-size table) 222)
      (assert-equal? (dynamic-table-ref table 1)
         '("location" "https://www.example.com"))
      (assert-equal? (dynamic-table-ref table 2)
         '("date" "Mon, 21 Oct 2013 20:13:21 GMT"))
      (assert-equal? (dynamic-table-ref table 3)
         '("cache-control" "private"))
      (assert-equal? (dynamic-table-ref table 4)
         '(":status" "302"))
      
      (assert-equal? (hpack-decode! context b2)
         '((":status" "307")
           ("cache-control" "private")
           ("date" "Mon, 21 Oct 2013 20:13:21 GMT")
           ("location" "https://www.example.com")))

      (assert= (dynamic-table-size table) 222)
      (assert-equal? (dynamic-table-ref table 1)
         '(":status" "307"))
      (assert-equal? (dynamic-table-ref table 2)
         '("location" "https://www.example.com"))
      (assert-equal? (dynamic-table-ref table 3)
         '("date" "Mon, 21 Oct 2013 20:13:21 GMT"))
      (assert-equal? (dynamic-table-ref table 4)
         '("cache-control" "private"))
      
      (assert-equal? (hpack-decode! context b3)
         '((":status" "200")
           ("cache-control" "private")
           ("date" "Mon, 21 Oct 2013 20:13:22 GMT")
           ("location" "https://www.example.com")
           ("content-encoding" "gzip")
           ("set-cookie"
              "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")))
      (assert= (dynamic-table-size table) 215)

      (assert-equal? (dynamic-table-ref table 1)
         '("set-cookie"
           "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
      (assert-equal? (dynamic-table-ref table 2)
         '("content-encoding" "gzip"))
      (assert-equal? (dynamic-table-ref table 3)
         '("date" "Mon, 21 Oct 2013 20:13:22 GMT"))))


(define-test-suite hpack-tests

   (test "cond-expand works with hpack"
      (assert-true (cond-expand
                      (hpack #t)
                      (else #f))))

   (test "series of non-huffman encoded requests"
      (let ((b1 (blob #x82 #x86 #x84 #x41 #x0f #x77 #x77 #x77 #x2e #x65 #x78 #x61
                   #x6d #x70 #x6c #x65 #x2e #x63 #x6f #x6d))
            (b2 (blob #x82 #x86 #x84 #xbe #x58 #x08 #x6e #x6f #x2d #x63 #x61 #x63
                   #x68 #x65))
            (b3 (blob #x82 #x87 #x85 #xbf #x40 #x0a #x63 #x75 #x73 #x74 #x6f #x6d
                   #x2d #x6b #x65 #x79 #x0c #x63 #x75 #x73 #x74 #x6f #x6d #x2d
                   #x76 #x61 #x6c #x75 #x65)))
         (test-series-of-3-requests b1 b2 b3)))

   (test "series of huffman encoded requests"
      (let ((b1 (blob #x82 #x86 #x84 #x41 #x8c #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                   #x6b #xa0 #xab #x90 #xf4 #xff))
            (b2 (blob #x82 #x86 #x84 #xbe #x58 #x86 #xa8 #xeb #x10 #x64 #x9c
                   #xbf))
            (b3 (blob #x82 #x87 #x85 #xbf #x40 #x88 #x25 #xa8 #x49 #xe9 #x5b #xa9
                   #x7d #x7f #x89 #x25 #xa8 #x49 #xe9 #x5b #xb8 #xe8 #xb4 #xbf)))

         (test-series-of-3-requests b1 b2 b3)))

   (test "series of non-huffman encoded responses"
      (let ((b1 (blob #x48 #x03 #x33 #x30 #x32 #x58 #x07 #x70 #x72 #x69 #x76 #x61
                   #x74 #x65 #x61 #x1d #x4d #x6f #x6e #x2c #x20 #x32 #x31 #x20
                   #x4f #x63 #x74 #x20 #x32 #x30 #x31 #x33 #x20 #x32 #x30 #x3a
                   #x31 #x33 #x3a #x32 #x31 #x20 #x47 #x4d #x54 #x6e #x17 #x68
                   #x74 #x74 #x70 #x73 #x3a #x2f #x2f #x77 #x77 #x77 #x2e #x65
                   #x78 #x61 #x6d #x70 #x6c #x65 #x2e #x63 #x6f #x6d))
            (b2 (blob #x48 #x03 #x33 #x30 #x37 #xc1 #xc0 #xbf))
            (b3 (blob #x88 #xc1 #x61 #x1d #x4d #x6f #x6e #x2c #x20 #x32 #x31 #x20
                   #x4f #x63 #x74 #x20 #x32 #x30 #x31 #x33 #x20 #x32 #x30 #x3a
                   #x31 #x33 #x3a #x32 #x32 #x20 #x47 #x4d #x54 #xc0 #x5a #x04
                   #x67 #x7a #x69 #x70 #x77 #x38 #x66 #x6f #x6f #x3d #x41 #x53
                   #x44 #x4a #x4b #x48 #x51 #x4b #x42 #x5a #x58 #x4f #x51 #x57
                   #x45 #x4f #x50 #x49 #x55 #x41 #x58 #x51 #x57 #x45 #x4f #x49
                   #x55 #x3b #x20 #x6d #x61 #x78 #x2d #x61 #x67 #x65 #x3d #x33
                   #x36 #x30 #x30 #x3b #x20 #x76 #x65 #x72 #x73 #x69 #x6f #x6e
                   #x3d #x31)))
         (test-series-of-3-responses b1 b2 b3)))

   (test "series of huffman-encoded responses"
      (let ((b1 (blob #x48 #x82 #x64 #x02 #x58 #x85 #xae #xc3 #x77 #x1a #x4b #x61
                   #x96 #xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05
                   #x95 #x04 #x0b #x81 #x66 #xe0 #x82 #xa6 #x2d #x1b #xff #x6e
                   #x91 #x9d #x29 #xad #x17 #x18 #x63 #xc7 #x8f #x0b #x97 #xc8
                   #xe9 #xae #x82 #xae #x43 #xd3))
            (b2 (blob #x48 #x83 #x64 #x0e #xff #xc1 #xc0 #xbf))
            (b3 (blob #x88 #xc1 #x61 #x96 #xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44
                   #xa8 #x20 #x05 #x95 #x04 #x0b #x81 #x66 #xe0 #x84 #xa6 #x2d
                   #x1b #xff #xc0 #x5a #x83 #x9b #xd9 #xab #x77 #xad #x94 #xe7
                   #x82 #x1d #xd7 #xf2 #xe6 #xc7 #xb3 #x35 #xdf #xdf #xcd #x5b
                   #x39 #x60 #xd5 #xaf #x27 #x08 #x7f #x36 #x72 #xc1 #xab #x27
                   #x0f #xb5 #x29 #x1f #x95 #x87 #x31 #x60 #x65 #xc0 #x03 #xed
                   #x4e #xe5 #xb1 #x06 #x3d #x50 #x07)))
         (test-series-of-3-responses b1 b2 b3)))



   (test "encoding and decoding a series of non-huffman-encoded requests"
      (let ((request-headers1 '((":method" "GET")
                                (":scheme" "http")
                                (":path" "/")
                                (":authority"
                                   "www.example.com")))
            (request-headers2 '((":method" "GET")
                                (":scheme" "http")
                                (":path" "/")
                                (":authority"
                                   "www.example.com")
                                ("cache-control" "no-cache")))
            (request-headers3 '((":method" "GET")
                                (":scheme" "https")
                                (":path" "/index.html")
                                (":authority" "www.example.com")
                                ("custom-key" "custom-value")))
            (context (make-hpack-context 65536)))
         
         (receive (b1-length b1)
            (hpack-encode! context request-headers1 :enable-huffman #f)
            (receive (b2-length b2)
               (hpack-encode! context request-headers2 :enable-huffman #f)
               (receive (b3-length b3)
                  (hpack-encode! context request-headers3 :enable-huffman #f)
                  (blob-shrink! b1 b1-length)
                  (blob-shrink! b2 b2-length)
                  (blob-shrink! b3 b3-length)
                  (test-series-of-3-requests b1 b2 b3))))))


   (test "encoding and decoding a series of huffman-encoded requests"
      (let ((request-headers1 '((":method" "GET")
                                (":scheme" "http")
                                (":path" "/")
                                (":authority"
                                   "www.example.com")))
            (request-headers2 '((":method" "GET")
                                (":scheme" "http")
                                (":path" "/")
                                (":authority"
                                   "www.example.com")
                                ("cache-control" "no-cache")))
            (request-headers3 '((":method" "GET")
                                (":scheme" "https")
                                (":path" "/index.html")
                                (":authority" "www.example.com")
                                ("custom-key" "custom-value")))
            (context (make-hpack-context 65536)))
         
         (receive (b1-length b1)
            (hpack-encode! context request-headers1 :enable-huffman #t)
            (receive (b2-length b2)
               (hpack-encode! context request-headers2 :enable-huffman #t)
               (receive (b3-length b3)
                  (hpack-encode! context request-headers3 :enable-huffman #t)
                  (blob-shrink! b1 b1-length)
                  (blob-shrink! b2 b2-length)
                  (blob-shrink! b3 b3-length)
                  (test-series-of-3-requests b1 b2 b3))))))


   (test "encoding and decoding a series of non-huffman-encoded responses"
      (let ((response-headers1 '((":status" "302")
                                 ("cache-control" "private")
                                 ("date" "Mon, 21 Oct 2013 20:13:21 GMT")
                                 ("location" "https://www.example.com")))
            (response-headers2 '((":status" "307")
                                 ("cache-control" "private")
                                 ("date" "Mon, 21 Oct 2013 20:13:21 GMT")
                                 ("location" "https://www.example.com")))
            (response-headers3 '((":status" "200")
                                 ("cache-control" "private")
                                 ("date" "Mon, 21 Oct 2013 20:13:22 GMT")
                                 ("location" "https://www.example.com")
                                 ("content-encoding" "gzip")
                                 ("set-cookie"
                                    "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")))
            
            (context (make-hpack-context 256)))
         
         (receive (b1-length b1)
            (hpack-encode! context response-headers1 :enable-huffman #f)
            (receive (b2-length b2)
               (hpack-encode! context response-headers2 :enable-huffman #f)
               (receive (b3-length b3)   
                  (hpack-encode! context response-headers3 :enable-huffman #f)
                  (blob-shrink! b1 b1-length)
                  (blob-shrink! b2 b2-length)
                  (blob-shrink! b3 b3-length)
                  (test-series-of-3-responses b1 b2 b3)
                  )))))

   (test "encoding and decoding a series of huffman-encoded responses"
      (let ((response-headers1 '((":status" "302")
                                 ("cache-control" "private")
                                 ("date" "Mon, 21 Oct 2013 20:13:21 GMT")
                                 ("location" "https://www.example.com")))
            (response-headers2 '((":status" "307")
                                 ("cache-control" "private")
                                 ("date" "Mon, 21 Oct 2013 20:13:21 GMT")
                                 ("location" "https://www.example.com")))
            (response-headers3 '((":status" "200")
                                 ("cache-control" "private")
                                 ("date" "Mon, 21 Oct 2013 20:13:22 GMT")
                                 ("location" "https://www.example.com")
                                 ("content-encoding" "gzip")
                                 ("set-cookie"
                                    "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1")))
            
            (context (make-hpack-context 256)))
         
         (receive (b1-length b1)
            (hpack-encode! context response-headers1 :enable-huffman #t)
            (receive (b2-length b2)
               (hpack-encode! context response-headers2 :enable-huffman #t)
               (receive (b3-length b3)   
                  (hpack-encode! context response-headers3 :enable-huffman #t)
                  (blob-shrink! b1 b1-length)
                  (blob-shrink! b2 b2-length)
                  (blob-shrink! b3 b3-length)
                  (test-series-of-3-responses b1 b2 b3))))))
   
   (test "hpack encoding of indexed header fields works"
      (let ((context1 (make-hpack-context 256))
            (context2 (make-hpack-context 256)))
         (receive (encoded-length blob)
            (hpack-encode! context1 '(("test" "value")))
            (assert-equal? (hpack-decode! context2 blob encoded-length)
               '(("test" "value")))
            ;(hpack-context-dynamic-table )
            )))


   (test "hpack encoding of indexed header fields works"
      (let ((context1 (make-hpack-context 65536))
            (context2 (make-hpack-context 65536))
            (context3 (make-hpack-context 65536)))
         (receive (encoded-length b)
            (hpack-encode! context1 '((":method" "GET")
                                      (":scheme" "http")
                                      (":path" "/")
                                      (":authority"
                                         "www.example.com")))
            (blob-shrink! b encoded-length)
            (assert-equal? b  (blob #x82 #x86 #x84 #x41 #x8c #xf1 #xe3 #xc2 #xe5 #xf2 #x3a
                                 #x6b #xa0 #xab #x90 #xf4 #xff))))))

(define (main args)
   (let ((tr (instantiate::terminal-test-runner (suite hpack-tests))))
      (if (test-runner-execute tr #t)
          0
          -1)))
