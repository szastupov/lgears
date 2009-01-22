(import (slib format))

(define opcodes '(LOAD_FAST LOAD_CONST LOAD_FUNC FUNC_CALL LOAD_ENV JUMP_IF_FALSE JUMP_IF_TRUE JUMP_TO RETURN UNARY_NOT))

(define (gen-header)
  (fold-left (lambda (idx op)
			   (display (format "#define ~a\t~a\n" op idx))
			   (+ idx 1))
			 0 opcodes)
  (display "\n#define OP_CASE(code) case code: return #code")
  (display "\nconst char* opcode_name(int code)\n{\n")
  (display "\tswitch (code) {\n")
  (for-each (lambda (op)
			  (display (format "\t\tOP_CASE(~a);\n" op)))
			opcodes)
  (display "\t}\n")
  (display "\treturn \"unknown\";\n}"))

(gen-header)

(assert (eq? 1 2))
