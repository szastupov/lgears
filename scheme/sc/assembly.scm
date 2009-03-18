#|
 | Copyright (C) 2009 - Stepan Zastupov
 | This program is free software; you can redistribute it and/or
 | modify it under the terms of the GNU General Public License
 | as published by the Free Software Foundation; either version 2
 | of the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with this program; if not, write to the Free Software
 | Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 |#

(library (sc assembly)
  (export assemble
          make-ilr make-i-func print-ilr)
  (import (rnrs)
          (sc opcode)
          (format))

  (define-record-type ilr
    (fields imports symbols code 
      strings entry-point))

  (define-record-type i-func
    (fields code size argc swallow
      heap? depth bindings bindmap))

  (define (print-ilr res)
    (display "ILR: \n")
    (format #t "Imports: ~a\n" (ilr-imports res))
    (format #t "Symbols: ~a\n" (ilr-symbols res))
    (format #t "Strings: ~s\n" (ilr-strings res))
    (format #t "Entry point: ~a\n" (ilr-entry-point res))
    (display "Code: \n")
    (for-each (lambda (x)
                (format #t "~a\n" (i-func-code x)))
              (ilr-code res)))

  (define (write-func-hdr mem . args)
    (fold-left (lambda (offset x)
                 (bytevector-u16-native-set! mem offset x)
                 (+ 2 offset))
               0 args))

  (define (bool->int b)
    (if b 1 0))

  (define (assemble-code func)
    (let* ((funhdr-size 18)
           (code (i-func-code func))
           (bcount (length (i-func-bindings func)))
           (bmcount (length (i-func-bindmap func)))
           (mem (make-bytevector
                  (+ funhdr-size
                     (* 2 (length code)) ; code
                     (* 2 bcount) ; bindings
                     bmcount)))) ; bindmap

      (define (assemble-main cur dbg stack-use stack-size offset)
        (if (null? cur)
          (values stack-size (reverse dbg))
          (let* ((cmd (car cur))
                 (nuse (+ stack-use (caddr cmd))))
            (bytevector-u8-set! mem offset (opcode (car cmd)))
            (bytevector-s8-set! mem (+ 1 offset) (cadr cmd))
            (assemble-main (cdr cur)
                           ;; Add debug symbols
                           (cons 
                             (string->utf8
                               (if (null? (cdddr cmd))
                                 "\x0;"
                                 (let ((sym (cadddr cmd)))
                                   (string-append
                                     (if (string? sym)
                                       sym
                                       (symbol->string sym))
                                     "\x0;"))))
                             dbg)
                           nuse
                           (max nuse stack-size)
                           (+ offset 2)))))

      (let* ((bindmap-offset
               (fold-left (lambda (offset bind)
                            (bytevector-u8-set! mem offset (car bind))
                            (bytevector-u8-set! mem (+ 1 offset) (cdr bind))
                            (+ offset 2))
                          funhdr-size (i-func-bindings func)))
             (code-offset
               (fold-left (lambda (offset bm)
                            (bytevector-u8-set! mem offset bm)
                            (+ 1 offset))
                          bindmap-offset (i-func-bindmap func))))
        (let-values (((res-size dbg) (assemble-main code '() 0 0 code-offset)))
          (write-func-hdr mem
                          (i-func-size func)    ; env size
                          (i-func-argc func)    ; argc
                          (bool->int (i-func-swallow func))
                          res-size              ; stack size
                          (length code)         ; op count
                          (bool->int (i-func-heap? func))  ; allocate env on heap?
                          (i-func-depth func)   ; depth
                          bcount                ; count of bindings
                          bmcount)              ; size of bindmap
          (values dbg mem)))))


  (define (assemble root path)

    (define (write-strings port strings)
      (let ((oldpos (port-position port)))
        (put-u8 port (length strings))
        (for-each (lambda (x)
                    (let ((utf8 (string->utf8 x)))
                      (put-bytevector port utf8)
                      (put-u8 port 0)))
                  strings)
        (- (port-position port) oldpos)))

    (define (number->u16 n)
      (let ((bv (make-bytevector 2)))
        (bytevector-u16-native-set! bv 0 n)
        bv))

    (let ((asm-port (open-file-output-port
                      path
                      (file-options no-fail)))
          (dbg-port (open-file-output-port
                      (string-append path ".dbg")
                      (file-options no-fail))))

      (define (write-dbg dbg)
        (let ((ssize (apply + (map bytevector-length dbg))))
          (put-bytevector dbg-port (number->u16 ssize))
          (for-each (lambda (x)
                      (put-bytevector dbg-port x))
                    dbg)))

      (let* ((hdr-size 16)
             (header (make-bytevector hdr-size)))
        (set-port-position! asm-port hdr-size)
        (bytevector-u32-native-set! header 0 (write-strings asm-port (ilr-imports root)))
        (bytevector-u32-native-set! header 4 (write-strings asm-port (ilr-symbols root)))
        (bytevector-u32-native-set! header 8 (write-strings asm-port (ilr-strings root)))
        (bytevector-u16-native-set! header 12 (length (ilr-code root)))
        (bytevector-u16-native-set! header 14 (ilr-entry-point root))

        (fold-left (lambda (idx func)
                     (let-values (((dbg code) (assemble-code func)))
                       (write-dbg dbg)
                       (put-bytevector asm-port code))
                     (+ 1 idx))
                   0 (ilr-code root))

        (set-port-position! asm-port 0)
        (put-bytevector asm-port header))

      (close-output-port asm-port)
      (close-output-port dbg-port)))
  )
