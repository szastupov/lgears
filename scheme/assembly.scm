(library (assembly)
  (export assemble)
  (import (rnrs) (opcode))

  (define-record-type func
	(fields id argc
	  (mutable stack-size)
	  (mutable stack-usage)))

  (define (assemble root port)
	(define (write-strings strings)
	  (let ((oldpos (port-position port)))
		(for-each (lambda (x)
					(put-bytevector port (string->utf8 x))
					(put-u8 port 0))
				  strings)
		(- (port-position port) oldpos)))

	(let ((undefs (assq 'undefs root))
		  (symbols (assq 'symbols root))
		  (code (assq 'code root)))
	  (set-port-position! port 8)
	  (let ((header (make-bytevector 8)))
		(bytevector-u32-native-set! header 0 (write-strings (cadr undefs)))
		(bytevector-u32-native-set! header 4 (write-strings (cadr symbols)))
		(set-port-position! port 0)
		(put-bytevector port header))
	  ))
  )
