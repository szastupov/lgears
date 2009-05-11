(library (core.forms)
  (export quote lambda define if set! begin syntax syntax-case
          with-syntax syntax-rules or and let let* cond when unless not)
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

  (display "coreforms loaded\n")

  )
