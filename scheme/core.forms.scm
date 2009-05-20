(library (core.forms)
  (export quote lambda define if set! begin syntax syntax-case
          with-syntax syntax-rules or and let let* cond case unquote unquote-splicing
          when unless not cons car cdr eq?)
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
		 (with-syntax (((ccl ...) (map clause #'(cl ...))))
		   (syntax (lambda (x) (syntax-case x (k ...) ccl ...))))))))

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
    (syntax-rules ()
      ((_ ((vars vals) ...) e1 e2 ...)
       ((lambda (vars ...)
          e1 e2 ...)
        vals ...))
      ((_ loop ((vars vals) ...) e1 e2 ...)
       (let ()
         (define loop (lambda (vars ...)
                        e1 e2 ...))
         (loop vals ...)))))

  (define-syntax let*
    (syntax-rules ()
      ((_ () e1 e2 ...)
       (let () e1 e2 ...))
      ((_ ((var1 val1) head ...)
          body1 body2 ...)
       (let ((var1 val1))
         (let* (head ...)
           body1 body2 ...)))))

  (define-syntax cond
    (lambda (x)
      (syntax-case x ()
        ((_ m1 m2 ...)
         (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
           (if (null? clauses)
               (syntax-case clause (else =>)
                 ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                 ((e0) (syntax (let ((t e0)) (if t t))))
                 ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t)))))
                 ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...))))
                 (_ (syntax-error x)))
               (with-syntax ((rest (f (car clauses) (cdr clauses))))
                 (syntax-case clause (else =>)
                   ((e0) (syntax (let ((t e0)) (if t t rest))))
                   ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t) rest))))
                   ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                   (_ (syntax-error x))))))))))

  (define-syntax case
    (lambda (x)
      (syntax-case x ()
        ((_ e m1 m2 ...)
         (with-syntax
             ((body (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
                      (if (null? clauses)
                          (syntax-case clause (else)
                            ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                            (((k ...) e1 e2 ...)
                             (syntax (if (memv t '(k ...)) (begin e1 e2 ...))))
                            (_ (syntax-error x)))
                          (with-syntax ((rest (f (car clauses) (cdr clauses))))
                            (syntax-case clause (else)
                              (((k ...) e1 e2 ...)
                               (syntax (if (memv t '(k ...))
                                           (begin e1 e2 ...)
                                           rest)))
                              (_ (syntax-error x))))))))
           (syntax (let ((t e)) body)))))))

  (define-syntax unquote
    (lambda (x)
      (syntax-error x "misplaced")))

  (define-syntax unquote-splicing
    (lambda (x)
      (syntax-error x "misplaced")))

  (define-syntax when
	(syntax-rules ()
	  ((when test result1 result2 ...)
	   (if test
		   (begin result1 result2 ...)))))

  (define-syntax unless
	(syntax-rules ()
	  ((unless test result1 result2 ...)
	   (if (not test)
		   (begin result1 result2 ...)))))

  (define-syntax not
    (lambda (x)
      (syntax-case x ()
        ((not expr) #'(if expr #f #t))
        (_ #'(lambda (x) (if x #f #t))))))

  (define-syntax cons
    (lambda (x)
      (syntax-case x ()
        ((_ a b) #'($cons a b))
        (_ #'(lambda (a b)
               ($cons a b))))))

  (define-syntax car
    (lambda (x)
      (syntax-case x ()
        ((_ a) #'($car a))
        (_ #'(lambda (a) ($car a))))))

  (define-syntax cdr
    (lambda (x)
      (syntax-case x ()
        ((_ a) #'($cdr a))
        (_ #'(lambda (a) ($cdr a))))))

  (define-syntax eq?
    (lambda (x)
      (syntax-case x ()
        ((_ a b) #'($eq? a b))
        (_ #'(lambda (a b)
               ($eq? a b))))))

  )
