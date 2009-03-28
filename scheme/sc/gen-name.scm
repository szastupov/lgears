(library (sc gen-name)
  (export gen-name)
  (import (rnrs))

  (define last-name 0)

  (define (gen-name)
    (let ((res (string-append "_var" (number->string last-name 16))))
      (set! last-name (+ last-name 1))
      (string->symbol res)))
  
  )