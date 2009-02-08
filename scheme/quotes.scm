(library (quotes)
  (export trquote trquasiquote)
  (import (rnrs))

  (define (self-eval? x)
	(or (number? x)
		(string? x)
		(char? x)
		(boolean? x)))

  ; Common transquote routine
  (define (trquote-common qv pfunc)
	(cons 'list 
		  (let loop ((cur qv))
			(cond ((null? cur)
				   '())
				  ((pair? cur)
				   (cons (let ((head (car cur)))
						   (cond ((pair? head)
								  (pfunc head))
								 ((self-eval? head)
								  head)
								 (else `(quote ,(car cur)))))
						 (loop (cdr cur))))
				  (else cur)))))

  ; Translate quotation to functions
  (define (trquote qv)
	(trquote-common qv trquote))

  ; Translate quoasiquotation to functions
  ; FIXME: implement splicing
  (define (trquasiquote qv)
	(trquote-common
	  qv
	  (lambda (head)
		(case (car head)
		  ((unquote)
		   (cadr head))
		  ((unquote-splicing)
		   (cadr head))
		  (else
			(trquasiquote head))))))
  )
