(library (coreforms)
  (export with-syntax syntax-rules or and let cond)
  (import ($builtin))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
        ((_ () e1 e2 ...)
         (syntax (begin e1 e2 ...)))
        ((_ ((out in)) e1 e2 ...)
         (syntax (syntax-case in () (out (begin e1 e2 ...)))))
        ((_ ((out in) ...) e1 e2 ...)
         (syntax (syntax-case (list in ...) ()
                   ((out ...) (begin e1 e2 ...))))))))

  (define-syntax syntax-rules
    (lambda (x)
      (define (clause y)
        (syntax-case y ()
          (((keyword . pattern) template)
           (syntax ((dummy . pattern) (syntax template))))
          (((keyword . pattern) fender template)
           (syntax ((dummy . pattern) fender (syntax template))))
          (_ (syntax-error x))))
      (syntax-case x ()
        ((_ (k ...) cl ...)
         (with-syntax (((ccl ...) (map clause #'(cl ...)))
                       ((kl ...) #'(k ...)))
           (syntax (lambda (x) (syntax-case x (kl ...) ccl ...))))))))

  (define-syntax or
    (syntax-rules ()
      ((_) #f)
      ((_ a) a)
      ((_ a b ...)
       (let ((t a))
         (if t t (or b ...))))))

  (define-syntax and
    (syntax-rules ()
      ((_) #t)
      ((_ a) a)
      ((_ a b ...)
       (if a (and b ...) #f))))

  (define-syntax let
    (lambda (x)
      (syntax-case x ()
        ((_ ((vars vals) ...) e1 e2 ...)
         #'((lambda (vars ...)
              e1 e2 ...)
            vals ...))
        ((_ loop ((vars vals) ...) e1 e2 ...)
         #'(let ((loop 'unspec))
             (set! loop (lambda (vars ...)
                          e1 e2 ...))
             (loop vals ...))))))

  (define-syntax cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))

  (display "!!!loaded!!\n")

  )