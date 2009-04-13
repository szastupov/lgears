(define-syntax when
  (lambda (x)
    (syntax-case x ()
      ((_ test e1 e2 ...) #'(if test (begin e1 e2 ...))))))

(define-syntax unless
  (lambda (x)
    (syntax-case x ()
      ((_ test e1 e2 ...) #'(if (not test) (begin e1 e2 ...))))))
