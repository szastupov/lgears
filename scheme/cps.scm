(import (rnrs))

(define orig '((define (f-aux n a)
				 (if (= n 0)
				   a
				   (f-aux (- n 1) (* n a))))
			   (define (factorial n)
				 (if (= n 0)
					 1
				   (* n (factorial (- n 1)))))
			   (define foo (lambda (x y) (+ x y (begin (display 12) (* 1 2 3)))))
			   (define (pyth x y)
				 (sqrt (+ (* x x) (* y y))))))

(define last-name 0)

(define (gen-name)
  (let ((res (string-append "val" (number->string last-name 16))))
	(set! last-name (+ last-name 1))
	(string->symbol res)))

(define (echo n)
  (display n)
  (newline))

(define (convert res node name)
  (if (pair? node)
	(case (car node)
	  ((lambda)
	   `(lambda (,@(cadr node) ,name)
		  ,(convert-body (cddr node) name)))

	  ((if)
	   (let* ((args (cdr node))
			  (predname (gen-name)))
		 (convert `(if ,predname
					 ,(convert '() (cadr args) name)
					 ,(convert '() (caddr args) name))
				  (car args) predname)))

	  ((begin)
	   (fold-left (lambda (prev x)
					(convert prev x (if (null? prev)
									  name
									  (gen-name))))
				  res (reverse (cdr node))))

	  (else
		(let* ((args (reverse (cdr node)))
			   (largs (map (lambda (x)
							 (if (pair? x)
							   (gen-name)
							   x))
						   args))
			   (tail (if (null? res)
					   name
					   `(lambda (,name) ,res)))
			   (expr `(,(car node) ,@(reverse largs) ,tail)))
		  (fold-left (lambda (prev x n)
					   (if (pair? x)
						 (convert prev x n)
						 prev))
					 expr args largs))))
	(if (null? res)
	  (list name node)
	  `(,node (lambda (,name) ,res)))))

(define (convert-define def)
  (cond ((pair? (car def))
		 (let ((name (gen-name)))
		   `(define ,(caar def)
			  (lambda (,@(cdar def) ,name)
				,(convert-body (cdr def) name)))))
		((pair? (cadr def))
		 `(define ,(car def)
			,(convert '() (cadr def) (gen-name))))
		(else def)))

(define (convert-body body name)
  (fold-left (lambda (prev x)
			   (if (eq? (car x) 'define)
				 (cons (convert-define (cdr x)) prev)
				 (convert prev x (if (null? prev)
								   name
								   (gen-name)))))
			 '() (reverse body)))

(pretty-print
  (convert-body orig (gen-name)))
