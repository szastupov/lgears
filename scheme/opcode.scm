(library (opcode)
  (export oplist oplist-for-each opcode)
  (import (rnrs))

  (define (oplist-for-each func oplist)
	(fold-left (lambda (idx op)
				 (func idx op)
				 (+ idx 1))
			   0 oplist))

  (define oplist '(
				   LOAD_CONST LOAD_FUNC LOAD_PARENT LOAD_LOCAL LOAD_SYM
				   LOAD_IMPORT LOAD_ENV
				   JUMP_IF_FALSE JUMP_IF_TRUE JUMP_FORWARD
				   FUNC_CALL RETURN SET_LOCAL))

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
