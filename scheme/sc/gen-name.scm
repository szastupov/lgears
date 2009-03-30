(library (sc gen-name)
  (export gen-name)
  (import (rnrs)
          (format))

  (define last-name 0)

  (define (gen-name-impl src)
    (let ((res (string-append src (number->string last-name 16))))
      (set! last-name (+ last-name 1))
      (string->symbol res)))

  (define gen-name
    (case-lambda
      ((x) (gen-name-impl (format "~a." x)))
      (() (gen-name-impl "_var."))))
  )
