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
  (fields parent tbl)
  (protocol
	(lambda (new)
	  (case-lambda
		(()
		 (new '() (make-eq-hashtable)))
		((prev)
		 (new prev (make-eq-hashtable)))
		((prev args)
		 (let ([ntbl (make-eq-hashtable)])
		   (fold-left (lambda (idx arg)
						(hashtable-set! ntbl arg `(LOAD_LOCAL . ,idx))
						(+ idx 1))
					  0 args)
		   (new prev ntbl)))))))

(define (env-lookup env name)
  (if (null? env)
	#f
	(let ([res (hashtable-ref (env-tbl env) name #f)])
	  (if res
		res
		(env-lookup (env-parent env) name)))))

(define-record-type sym-table
  (fields table (mutable count))
  (protocol
	(lambda (new)
	  (lambda () (new (make-eq-hashtable) 0)))))

(define (sym-table-insert stbl sym)
  (let* ([tbl (sym-table-table stbl)]
		 [res (hashtable-ref tbl sym #f)])
	(if res
	  res
	  (begin
		(set! res (sym-table-count stbl))
		(hashtable-set! tbl sym res)
		(sym-table-count-set! stbl (+ res 1))
		res))))

; Translate quotation to functions
(define (trquote qv)
  (if (null? qv)
	''()
	(let ([head (car qv)])
	  `(cons ,(cond  ((pair? head) (trquote head))
					 ((number? head) head)
					 (else `(quote ,head)))
			 ,(trquote (cdr qv))))))


(define (start-compile root)
  (let ([undefs (make-sym-table)]
		[symbols (make-sym-table)]
		[funcs '()])

	(define (compile-seq env seq)
	  (map (lambda (x)
			 (compile env x))
		   seq))

	(define (compile-func parent args body)
	  (let* ([env (make-env parent args)]
			 [compiled (compile-seq env body)])
		`(FUNC ,@compiled)))

	(define (compile-if env node)
	  (let ([pred (compile env (car node))]
			[if-clause (compile env (cadr node))]
			[else-clause (compile env (caddr node))])
		`(BRANCH ,pred ,if-clause ,else-clause)))

	(define (compile-args env args)
	  (let loop ([cur args]
				 [idx 0]
				 [res '()])
		(if (null? cur)
		  (cons idx res)
		  (loop (cdr cur) 
				(+ idx 1)
				(cons (compile env (car cur)) res)))))

	(define (compile-call env node)
	  (let ([func (compile env (car node))]
			[args (compile-args env (cdr node))])
		`(CALL ,(car args) ,func ,@(cdr args))))

	(define (compile-macro node)
	  (let ([name (car node)]
			[ttype (caadr node)]
			[tbody (cdadr node)])
		(case ttype
		  [(syntax-rules)
		   (syntax-rules-compile name tbody)]
		  [else (error 'compile-macro "Unknown transformer" ttype)])))

	(define (compile env node)
	  (cond ((pair? node)
			 (case (car node)
			   [(lambda)	(compile-func env (cadr node) (cddr node))]
			   [(if)		(compile-if env (cdr node))]
			   [(begin)		(compile-seq env (cdr node))]
			   [(define-syntax)	(compile-macro (cdr node))]
			   [(quote)		(let ([qv (cadr node)])
							  (if (pair? qv)
								(compile env (trquote qv))
								`(LOAD_SYM ,(sym-table-insert undefs qv))))]
			   [else		(compile-call env node)]))
			((number? node)
			 (cons 'NUMBER node))
			((string? node)
			 (cons 'STRING node))
			(else
			  (let ([res (env-lookup env node)])
				(if res
				  res
				  (cons 'LOAD_UNDEF (sym-table-insert undefs node)))))))

	(compile (make-env) root)
	))

(let ([res (start-compile
			 `(lambda (x y) (if x x (+ y 1)))
;			 ''(one two three three)
			 )])
  (display "ILR: ")
  (display res)
  (newline)
  )