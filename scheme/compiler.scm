#!r6rs
(import (rnrs)
		(syntax-rules)
		(quotes)
		(assembly))

(define (set-func-args! ntbl args)
  (let loop ((idx 0) (lst args))
	(cond
	  ((null? lst)
	   idx)
	  ((symbol? lst)
	   (hashtable-set! ntbl lst idx)
	   (+ idx 1))
	  ((pair? lst)
	   (hashtable-set! ntbl (car lst) idx)
	   (loop (+ idx 1) (cdr lst)))
	  (else
		(error 'make-env "unexpected" lst)))))

;; Environment of current-compiling function
(define-record-type env
  (fields parent tbl (mutable size) argc)
  (protocol
	(lambda (new)
	  (case-lambda
		(()
		 (new '() (make-eq-hashtable) 0 0))
		((prev args)
		 (let* ((ntbl (make-eq-hashtable))
				(nargc (set-func-args! ntbl args)))
		   (new prev ntbl nargc nargc)))))))

(define (env-define env name)
  (let ((size (env-size env)))
	(hashtable-set! (env-tbl env) name size)
	(env-size-set! env (+ size 1))))

(define (env-lookup env name)
  (let loop ((step 0)
			 (cur-env env))
	(if (null? cur-env)
	  #f
	  (let ((res (hashtable-ref (env-tbl cur-env) name #f)))
		(if res
		  (if (zero? step)
			`(LOCAL . ,res)
			`(PARENT ,step ,res))
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
	  res
	  (begin
		(set! res (sym-table-count stbl))
		(hashtable-set! tbl sym res)
		(sym-table-count-set! stbl (+ res 1))
		res))))

;; Return list of keys sorted by value (index)
;;
;; As order of returned hash values is not specified, we have
;; to guaranty that everything is ok.
(define (symtable->list stbl)
  (let-values (((keys vals) (hashtable-entries (sym-table-table stbl))))
	(map (lambda (x)
		   (symbol->string (car x)))
		 (list-sort (lambda (x y) (< (cdr x) (cdr y)))
					(vector->list (vector-map cons keys vals))))))


(define-record-type store
  (fields (mutable head) (mutable count))
  (protocol
	(lambda (new)
	  (lambda () (new '() 0)))))

(define (store-push! store val)
  (let ((res (store-count store)))
	(store-count-set! store (+ 1 res))
	(store-head-set! store (cons val (store-head store)))
	res))

(define (map-append proc lst)
  (if (null? lst)
	'()
	(append (proc (car lst)) (map-append proc (cdr lst)))))

(define (start-compile root)
  (let ((undefs (make-sym-table))
		(symbols (make-sym-table))
		(code-store (make-store)))

	(define (compile-body env body)
	  (define (defination? x)
		(and (pair? x) (eq? (car x) 'define)))
	  (define (defination-name def)
		(let ((name (cadr def)))
		  (if (pair? name)
			(car name)
			name)))
	  (let-values (((defines expressions) (partition defination? body)))
		(for-each (lambda (def) (env-define env (defination-name def)))
				  defines)
		(let ((init (map-append (lambda (def)
								  `(,(if (pair? (cadr def))
									   (compile-func env (cdadr def) (cddr def))
									   (compile env (caddr def)))
									 (SET_LOCAL ,(cdr (env-lookup env (defination-name def))) -1)))
								defines))
			  (rest (map-append (lambda (expr)
								  (compile env expr))
								expressions)))
		  (append init rest))))

	(define (compile-func parent args body)
	  (let* ((env (make-env parent args))
			 (compiled (compile-body env body)))
		`(LOAD_FUNC ,(store-push! code-store
								  (list compiled (env-size env) (env-argc env))) 1)))

	(define (compile-if env node)
	  (let ((pred (compile env (car node)))
			(then-clause (compile env (cadr node)))
			(else-clause (compile env (caddr node))))
		`(,pred
		   (JUMP_IF_FALSE ,(+ (length then-clause) 1) 0)
		   ,@then-clause
		   (JUMP_FORWARD ,(length else-clause) 0)
		   ,@else-clause)))

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
	  (let* ((func (compile env (car node)))
			 (args (compile-args env (cdr node)))
			 (argc (car args)))
		`(,@(cdr args)
		   ,func
		   (FUNC_CALL ,argc ,(+ (- argc) 1) )))) ; FIXME check stack usage counting

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
		`(LOAD_SYM ,(sym-table-insert symbols qv) 1)))

	(define (compile-seq env seq)
	  (map (lambda (x)
			 (compile env x))
		   seq))

	(define (compile-assigment env node)
	  (let* ((name (car node))
			 (slot (env-lookup env name)))
		(if (not slot)
		  (error 'compile-assigment "undefined variable" name)
		  `(SET ,slot ,(compile env (cadr node))))))

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
				(error 'compile "misplaced defination"))
			   ((set!)
				(compile-assigment env (cdr node)))
			   (else
				 (compile-call env node))))
			((number? node)
			 (cons 'NUMBER node))
			((string? node)
			 (cons 'STRING node))
			(else
			  (let ((res (env-lookup env node)))
				(if res
				  (if (eq? (car res) 'LOCAL)
					`(LOAD_LOCAL ,(cdr res) 1)
					`(LOAD_PARENT 0 0)) ; FIXME
				  `(LOAD_IMPORT ,(sym-table-insert undefs node) 1))))))

	(let ((entry-point (compile-func (make-env) '() root)))
	  `((undefs	,(reverse (symtable->list undefs)))
		(symbols ,(reverse (symtable->list symbols)))
		(code ,(reverse (store-head code-store)))
		(entry ,entry-point)))))

(let ((res (start-compile
			 '(
			   ((lambda (x) (display x)) 'foobar)
			   )
			 ;'('(one two three four))
			 ;'(`(one ,two three "four"))
			 )))
  (display "ILR: ")
  (display res)
  (newline)
  (display "Assembly output:\n")
  (let ((port (open-file-output-port "/tmp/assembly" (file-options no-fail))))
	(assemble res port)
	(close-output-port port))
  )
