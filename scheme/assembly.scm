(library (assembly)
  (export assemble
          make-ilr make-i-func print-ilr)
  (import (rnrs)
          (opcode)
          (slib format))

  (define-record-type ilr
    (fields imports symbols code entry-point))

  (define-record-type i-func
    (fields code size argc heap? depth))

  (define (print-ilr res)
    (display "ILR: \n")
    (display (format "Imports: ~a\n" (ilr-imports res)))
    (display (format "Symbols: ~a\n" (ilr-symbols res)))
    (display (format "Entry point: ~a\n" (ilr-entry-point res)))
    (display "Code: \n")
    (for-each (lambda (x)
                (display (format "~a\n" (i-func-code x))))
              (ilr-code res)))

  (define funhdr-size 24)

  (define (assemble-code func)
    (display "Assembling " ) (display func) (newline)
    (let* ((code (i-func-code func))
           (mem (make-bytevector (+ funhdr-size
                                    (* 2 (length code)))))
           (res-size
             (let loop ((cur code)
                        (stack-use 0)
                        (stack-size 0)
                        (offset funhdr-size))
               (if (null? cur)
                 stack-size
                 (let* ((cmd (car cur))
                        (nuse (+ stack-use (caddr cmd))))
                   ;(display (format "opcode ~a = ~a\n" (car cmd) (opcode (car cmd))))
                   (bytevector-u8-set! mem offset (opcode (car cmd)))
                   (bytevector-s8-set! mem (+ 1 offset) (cadr cmd))
                   (loop (cdr cur)
                         nuse
                         (max nuse stack-size)
                         (+ offset 2)))))))
      (bytevector-u32-native-set! mem 0 (i-func-size func)) ; env size 
      (bytevector-u32-native-set! mem 4 (i-func-argc func)) ; argc
      (bytevector-u32-native-set! mem 8 res-size) ; stack size
      (bytevector-u32-native-set! mem 12 (length code)) ; op count
      (bytevector-u32-native-set! mem 16 (if (i-func-heap? func) 1 0)) ; allocate env on heap?
      (bytevector-u32-native-set! mem 20 (i-func-depth func)) ; function depth
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

    (let ((hdr-size 16))
      (set-port-position! port hdr-size)
      (let ((header (make-bytevector hdr-size)))
        (bytevector-u32-native-set! header 0 (write-strings (ilr-imports root)))
        (bytevector-u32-native-set! header 4 (write-strings (ilr-symbols root)))
        (bytevector-u32-native-set! header 8 (length (ilr-code root)))
        (bytevector-u32-native-set! header 12 (ilr-entry-point root))
        (for-each (lambda (func)
                    (put-bytevector port (assemble-code func)))
                  (ilr-code root))
        (set-port-position! port 0)
        (put-bytevector port header))
      ))
  )
