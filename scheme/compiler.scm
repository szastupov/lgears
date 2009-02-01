#!r6rs
(import (rnrs)
		(syntax-rules)
		(opcode)
		(trace))

;;
;; Compiler todo
;;	* Expand macros
;;	* Determine, does function use parent env (continuatin) or not (simple func)
;;	* Generate intermediate assembly reprsesentation and send it to assembler
;;	* Compile backquotes
;;	* Detect dot-notation and decide apply method
;;	* Determine and compile tail-calls


;; Environment of current-compiling function
(define-record-type env
  (fields parent tbl argc)
  (protocol
	(lambda (new)
	  (case-lambda
		(()
		 (new '() (make-eq-hashtable) 0))
		((prev)
		 (new prev (make-eq-hashtable) 0))
		((prev args)
		 (let* ((ntbl (make-eq-hashtable))
				(nargc (fold-left (lambda (idx arg)
									(hashtable-set! ntbl arg idx)
									(+ idx 1))
								  0 args)))
		   (new prev ntbl nargc)))))))

(define (env-lookup env name)
  (let loop ((step 0)
			 (cur-env env))
	(if (null? cur-env)
	  #f
	  (let ((res (hashtable-ref (env-tbl cur-env) name #f)))
		(if res
		  (if (zero? step)
			`(LOAD_LOCAL ,res)
			`(LOAD_PARENT ,step ,res))
		  (loop (+ step 1) (env-parent cur-env)))))))

(define-record-type sym-table
  (fields table (mutable count))
  (protocol
	(lambda (new)
	  (lambda () (new (make-eq-hashtable) 0)))))

(define (sym-table-insert stbl sym)
  (let* ((tbl (sym-table-table stbl))
		 (res (hashtable-ref tbl sym #f)))
	(if res
	  (begin
		(set! res (sym-table-count stbl))
		(hashtable-set! tbl sym res)
		(sym-table-count-set! stbl (+ res 1))
		res))
	sym ;FIXME
	))

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

(define (start-compile root)
  (let ((undefs (make-sym-table))
		(symbols (make-sym-table)))

	(define (compile-seq env seq)
	  (map (lambda (x)
			 (compile env x))
		   seq))

	(define (compile-func parent args body)
	  (let* ((env (make-env parent args))
			 (compiled (compile-seq env body)))
		`(FUNC ,@compiled)))

	(define (compile-if env node)
	  (let ((pred (compile env (car node)))
			(if-clause (compile env (cadr node)))
			(else-clause (compile env (caddr node))))
		`(BRANCH ,pred ,if-clause ,else-clause)))

	(define (compile-args env args)
	  (let loop ((cur args)
				 (idx 0)
				 (res '()))
		(if (null? cur)
		  (cons idx res)
		  (loop (cdr cur) 
				(+ idx 1)
				(cons (compile env (car cur)) res)))))

	(define (compile-call env node)
	  (let ((func (compile env (car node)))
			(args (compile-args env (cdr node))))
		`(CALL ,(car args) ,func ,@(cdr args))))

	(define (compile-macro node)
	  (let ((name (car node))
			(ttype (caadr node))
			(tbody (cdadr node)))
		(case ttype
		  ((syntax-rules)
		   (syntax-rules-compile name tbody))
		  (else (error 'compile-macro "Unknown transformer" ttype)))))

	(define (compile-quote env qv transform)
	  (if (pair? qv)
		(compile env (transform qv))
		`(LOAD_SYM ,(sym-table-insert symbols qv))))

	(define (compile env node)
	  (cond ((pair? node)
			 (case (car node)
			   ((lambda)
				(compile-func env (cadr node) (cddr node)))
			   ((if)
				(compile-if env (cdr node)))
			   ((begin)
				(compile-seq env (cdr node)))
			   ((define-syntax)
				(compile-macro (cdr node)))
			   ((quote)
				(compile-quote env (cadr node) trquote))
			   ((quasiquote)
				(compile-quote env (cadr node) trquasiquote))
			   ((define)
				(if (pair? (cadr node))
				  (compile-func env (cdadr node) (cddr node))))
			   (else
				 (compile-call env node))))
			((number? node)
			 (cons 'NUMBER node))
			((string? node)
			 (cons 'STRING node))
			(else
			  (let ((res (env-lookup env node)))
				(if res
				  res
				  (cons 'LOAD_UNDEF (sym-table-insert undefs node)))))))

	(compile (make-env) root)
	))

(let ((res (start-compile
			 '(lambda (x . y) (lambda (z) (+ x y z)))
			 ;'(define (foo x y) (if x x (foo y 1)))
			 ;''(one two three four)
			 ;'`(one ,two three "four")
			 )))
  (display "ILR: ")
  (display res)
  (newline)
  )
