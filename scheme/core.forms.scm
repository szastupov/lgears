(library (core.forms)
  (export quote lambda define if set! begin syntax syntax-case
          with-syntax syntax-rules or and let cond when unless not caaaar cdaaar
          cadaar cddaar caadar cdadar caddar cdddar caaadr cdaadr cadadr cddadr
          caaddr cdaddr cadddr cddddr caaar cdaar cadar cddar caadr cdadr caddr
          cdddr caar cdar cadr cddr assq)
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

  (define (caaaar x) (car (car (car (car x)))))
  (define (cdaaar x) (cdr (car (car (car x)))))
  (define (cadaar x) (car (cdr (car (car x)))))
  (define (cddaar x) (cdr (cdr (car (car x)))))
  (define (caadar x) (car (car (cdr (car x)))))
  (define (cdadar x) (cdr (car (cdr (car x)))))
  (define (caddar x) (car (cdr (cdr (car x)))))
  (define (cdddar x) (cdr (cdr (cdr (car x)))))
  (define (caaadr x) (car (car (car (cdr x)))))
  (define (cdaadr x) (cdr (car (car (cdr x)))))
  (define (cadadr x) (car (cdr (car (cdr x)))))
  (define (cddadr x) (cdr (cdr (car (cdr x)))))
  (define (caaddr x) (car (car (cdr (cdr x)))))
  (define (cdaddr x) (cdr (car (cdr (cdr x)))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))
  (define (cddddr x) (cdr (cdr (cdr (cdr x)))))
  (define (caaar x) (car (car (car x))))
  (define (cdaar x) (cdr (car (car x))))
  (define (cadar x) (car (cdr (car x))))
  (define (cddar x) (cdr (cdr (car x))))
  (define (caadr x) (car (car (cdr x))))
  (define (cdadr x) (cdr (car (cdr x))))
  (define (caddr x) (car (cdr (cdr x))))
  (define (cdddr x) (cdr (cdr (cdr x))))
  (define (caar x) (car (car x)))
  (define (cdar x) (cdr (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cddr x) (cdr (cdr x)))

  (define (assq key lst)
    (let loop ((cur lst))
      (cond ((null? cur) #f)
            ((eq? (caar cur) key)
             (cdar cur))
            (else
             (loop (cdr cur))))))

  #|
  ;; Generator
  (define (cXr vars)
    (if (null? vars)
        'x
        (list (format "c~ar" (car vars))
              (cXr (cdr vars)))))

  (define (print-define res)
    (format #t "(define (c~ar x) ~a)\n"
            (apply string-append res)
            (cXr res)))

  (define (print-name res)
    (format #t "c~ar " (apply string-append res)))

  (define (gen pf n)
    (let gen ((n n)
              (res '()))
      (if (= n 0)
          (pf res)
          (begin
            (gen (- n 1) (cons "a" res))
            (gen (- n 1) (cons "d" res))))))

  (define (gencXr pf)
    (do ((i 4 (- i 1)))
        ((= i 1))
      (gen pf i)))

  (gencXr print-name)
  (gencXr print-define)
  |#

  (define-syntax not
    (lambda (x)
      (syntax-case x ()
        ((not expr) #'(if expr #f #t))
        (_ #'(lambda (x) (if x #f #t))))))

  (display "coreforms loaded\n")

  )
