(library (assembly)
  (export assemble)
  (import (rnrs))

  (define-record-type sym-table
	(fields
	  table
	  (mutable count))
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

  (define (assemble root)
	(let ([undefs (make-sym-table)]
		  [symbols (make-sym-table)])

	  (define (assemble-call node)
		(let ([argc (car node)]
			  [func (cadr node)]
			  [args (cddr node)])
		  (for-each (lambda (x) (display (dispatch x)) (newline)) args)
		  (dispatch func)))

	  (define (dispatch node)
		(case (car node)
		  [(FUNC)
		   "got func"]

		  [(CALL)
		   (assemble-call (cdr node))]

		  [(UNDEF)
		   (let ([idx (sym-table-insert undefs (cdr node))])
			 `(LOAD_UNDEF ,idx))]

		  [(SYM)
		   (let ([idx (sym-table-insert symbols (cdr node))])
			 `(LOAD_SYM ,idx))]

		  (else (error 'dispatch "Unknown ILR command" (car node)))))

	  (dispatch root)
	  ))
  )
