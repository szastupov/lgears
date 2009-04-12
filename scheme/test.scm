(define-syntax or
  (lambda (x)
    (syntax-case x ()
      ((_) #'#f)
      ((_ a) #'a)
      ((_ a b ...)
       #'(let ((t a))
           (if t t (or b ...)))))))

(define-syntax and
  (lambda (x)
    (syntax-case x ()
      ((_) #'#t)
      ((_ a) #'a)
      ((_ a b ...)
       #'(if a (and b ...) #f)))))

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
  (lambda (x)
    (syntax-case x (else =>)
      ((cond (else result1 result2 ...))
       #'(begin result1 result2 ...))
      ((cond (test => result))
       #'(let ((temp test))
           (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       #'(let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
      ((cond (test)) #'test)
      ((cond (test) clause1 clause2 ...)
       #'(let ((temp test))
           (if temp
               temp
               (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       #'(if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       #'(if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...))))))

(define-syntax when
  (lambda (x)
    (syntax-case x ()
      ((_ test e1 e2 ...) #'(if test (begin e1 e2 ...))))))

(define-syntax unless
  (lambda (x)
    (syntax-case x ()
      ((_ test e1 e2 ...) #'(if (not test) (begin e1 e2 ...))))))

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


(cond ((and foo bar) 'test 'ttt)
      ((bar? x) => (lambda (x) (cdr x)))
      (else 'fuck))