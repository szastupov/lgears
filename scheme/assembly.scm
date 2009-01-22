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

  (define (assemble tree)
	(let ([undefs (make-sym-table)]
		  [symbols (make-sym-table)])
	  (display tree)
	  ))
  )
