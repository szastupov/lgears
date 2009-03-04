(library (assembly)
  (export assemble
          make-ilr make-i-func print-ilr)
  (import (rnrs)
          (opcode)
          (format))

  (define-record-type ilr
    (fields imports symbols code 
      strings entry-point))

  (define-record-type i-func
    (fields code size argc heap?
      depth bindings bindmap))

  (define (print-ilr res)
    (display "ILR: \n")
    (display (format "Imports: ~a\n" (ilr-imports res)))
    (display (format "Symbols: ~a\n" (ilr-symbols res)))
    (display (format "Strings: ~s\n" (ilr-strings res)))
    (display (format "Entry point: ~a\n" (ilr-entry-point res)))
    (display "Code: \n")
    (for-each (lambda (x)
                (display (format "~a\n" (i-func-code x))))
              (ilr-code res)))

  (define (write-func-hdr mem . args)
    (fold-left (lambda (offset x)
                 (bytevector-u16-native-set! mem offset x)
                 (+ 2 offset))
               0 args))

  (define (assemble-code func)
    (define funhdr-size 16)
    ;(display "Assembling " ) (display func) (newline)
    (let* ((code (i-func-code func))
           (bcount (length (i-func-bindings func)))
           (bmcount (length (i-func-bindmap func)))
           (mem (make-bytevector (+ funhdr-size
                                    (* 2 (length code)) ; code
                                    (* 2 bcount) ; bindings
                                    bmcount))) ; bindmap
           (bindmap-offset
             (fold-left (lambda (offset bind)
                          (bytevector-u8-set! mem offset (car bind))
                          (bytevector-u8-set! mem (+ 1 offset) (cdr bind))
                          (+ offset 2))
                        funhdr-size (i-func-bindings func)))
           (code-offset
             (fold-left (lambda (offset bm)
                          (bytevector-u8-set! mem offset bm)
                          (+ 1 offset))
                        bindmap-offset (i-func-bindmap func)))
           (res-size
             (let loop ((cur code)
                        (stack-use 0)
                        (stack-size 0)
                        (offset code-offset))
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
      (write-func-hdr mem
                      (i-func-size func)    ; env size
                      (i-func-argc func)    ; argc
                      res-size              ; stack size
                      (length code)         ; op count
                      (if (i-func-heap? func) 1 0)  ; allocate env on heap?
                      (i-func-depth func)   ; depth
                      bcount                ; count of bindings
                      bmcount)              ; size of bindmap
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
        (bytevector-u32-native-set! header 8 (write-strings (ilr-strings root)))
        (bytevector-u16-native-set! header 12 (length (ilr-code root)))
        (bytevector-u16-native-set! header 14 (ilr-entry-point root))
        (for-each (lambda (func)
                    (put-bytevector port (assemble-code func)))
                  (ilr-code root))
        (set-port-position! port 0)
        (put-bytevector port header))
      ))
  )
