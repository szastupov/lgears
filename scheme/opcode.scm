(library (opcode)
  (export oplist oplist-for-each opcode)
  (import (rnrs))

  #|
   | DO NOT FORGET TO RUN make regen-opcode after updating it
   |#

  (define oplist '((LOAD_FUNC . "Load function local to module")
                   (LOAD_CLOSURE . "Make closure object")
                   (LOAD_LOCAL . "Load object from frame-local area")
                   (LOAD_BIND . "Load object form binding")
                   (LOAD_SYM . "Load predefined symbol local to module")
                   (LOAD_BOOL . "Push boolean value")
                   (LOAD_IMPORT . "Load object from module import table")
                   (JUMP_IF_FALSE . "Jump if false")
                   (JUMP_IF_TRUE . "Jump if true")
                   (JUMP_FORWARD . "Jump forward")
                   (FUNC_CALL . "Call function")
                   (SET_LOCAL . "Assign new value to local binding")
                   (SET_BIND . "Asign new value to non-local binding")))

  (define (oplist-for-each func)
    (fold-left (lambda (idx op)
                 (func idx op)
                 (+ idx 1))
               0 oplist))

  (define (make-opcode-table)
    (let ([tbl (make-eq-hashtable)])
      (oplist-for-each (lambda (idx op)
                         (hashtable-set! tbl (car op) idx)))
      tbl))

  (define optable (make-opcode-table))

  (define (opcode sym)
    (if (hashtable-contains? optable sym)
      (hashtable-ref optable sym #f)
      (error 'opcode "unknown opcode" sym)))
  )
