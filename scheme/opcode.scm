(library (opcode)
  (export oplist oplist-for-each opcode)
  (import (rnrs)
		  (slib format))

  (define (oplist-for-each func oplist)
	(fold-left (lambda (idx op)
				 (func idx op)
				 (+ idx 1))
			   0 oplist))

  (define oplist '(LOAD_FAST LOAD_CONST LOAD_FUNC FUNC_CALL
							 LOAD_ENV JUMP_IF_FALSE JUMP_IF_TRUE 
							 JUMP_TO RETURN UNARY_NOT))

  (define (make-opcode-table)
	(let ([tbl (make-eq-hashtable)])
	  (oplist-for-each (lambda (idx op)
						 (hashtable-set! tbl op idx))
					   oplist)
	  tbl))

  (define optable (make-opcode-table))

  (define (opcode sym)
	(if (hashtable-contains? optable sym)
	  (hashtable-ref optable sym #f)
	  (error opcode "unknown opcode" sym)))
  )
