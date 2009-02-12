#!r6rs
(import (rnrs)
		(slib format)
		(opcode))

;;;
;;; It's ugly, I know :\
;;;

(define (with-header path func)
  (let ((port (open-file-output-port path
									 (file-options no-fail)
									 (buffer-mode block)
									 (native-transcoder))))
	(display (format "Generating header ~a ...\n" path))
	(func port)
	(close-port port)))

(with-header
  "opcodes.h"
  (lambda (port)
	(display "#ifndef OPCODES_H\n#define OPCODES_H\n\n" port)
	(display "/* Generated by gen-header.scm, do not edit. */\n\n" port)
	(oplist-for-each (lambda (idx op)
					   (display (format "#define ~a\t~a\t/* ~a */\n" (car op) idx (cdr op)) port)))
	(display "\n#define OP_CASE(code) case code: return #code" port)
	(display "\n\nconst char* opcode_name(int code)\n{\n" port)
	(display "\tswitch (code) {\n" port)
	(for-each (lambda (op)
				(display (format "\t\tOP_CASE(~a);\n" (car op)) port))
			  oplist)
	(display "\t}\n" port)
	(display "\treturn \"unknown\";\n}\n" port)

	(display "\n#endif" port)))

(with-header
  "opcode_targets.h"
  (lambda (port)

	(display "#ifndef OPCODE_TARGETS_H\n#define OPCODE_TARGETS_H\n\n" port)
	(display "static const void* opcode_targets[] = {\n" port)
	(oplist-for-each (lambda (idx op)
					   (display (format "\t&&TARGET_~a,\n" (car op)) port)))
	(display "};\n\n#endif" port)))
