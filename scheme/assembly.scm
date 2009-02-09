(library (assembly)
  (export assemble)
  (import (rnrs) (opcode))

  (define (assemble-code func)
	(display "Assembling " ) (display func) (newline)
	(let* ((code (car func))
		   (mem (make-bytevector (+ 16 (* 2 (length code)))))
		   (res-size
			 (let loop ((cur code)
						(stack-use 0)
						(stack-size 0)
						(offset 16))
			   (if (null? cur)
				 stack-size
				 (let* ((cmd (car cur))
						(nuse (+ stack-use (caddr cmd))))
				   (bytevector-u8-set! mem offset (opcode (car cmd)))
				   (bytevector-s8-set! mem (+ 1 offset) (cadr cmd))
				   (loop (cdr cur)
						 nuse
						 (max nuse stack-size)
						 (+ offset 2)))))))
	  (bytevector-u32-native-set! mem 0 (cadr func)) ; env size 
	  (bytevector-u32-native-set! mem 4 (caddr func)) ; argc
	  (bytevector-u32-native-set! mem 8 res-size) ; stack size
	  (bytevector-u32-native-set! mem 12 (length code)) ; op count
	  mem))


  (define (assemble root port)
	(define (write-strings strings)
	  (let ((oldpos (port-position port)))
		(put-u8 port (length strings))
		(for-each (lambda (x)
					(let ((utf8 (string->utf8 x)))
					  (put-u8 port (bytevector-length utf8))
					  (put-bytevector port utf8)
					  (put-u8 port 0)))
				  strings)
		(- (port-position port) oldpos)))

	(let ((undefs (cadr (assq 'undefs root)))
		  (symbols (cadr (assq 'symbols root)))
		  (code	(cadr (assq 'code root)))
		  (hdr-size 12))
	  (set-port-position! port hdr-size)
	  (let ((header (make-bytevector hdr-size)))
		(bytevector-u32-native-set! header 0 (write-strings undefs))
		(bytevector-u32-native-set! header 4 (write-strings symbols))
		(bytevector-u32-native-set! header 8 (length code))
		(for-each (lambda (func)
					(put-bytevector port (assemble-code func)))
				  code)
		(set-port-position! port 0)
		(put-bytevector port header))
	  ))
  )
