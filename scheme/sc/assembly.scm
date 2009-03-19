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
          (cpacked)
          (debug)
          (format))

  (define-record-type ilr
    (fields imports symbols code 
      strings entry-point))

  (define-record-type i-func
    (fields code size argc swallow
      heap? depth bindings bindmap))

  (define-cpacked
    header
    (u32 import-size)
    (u32 symbols-size)
    (u32 strings-size)
    (u16 fun-count)
    (u16 entry-point))

  (define-cpacked
    fun-hdr
    (u16 env-size)
    (u16 argc)
    (u16 swallow)
    (u16 stack-size)
    (u16 op-count)
    (u16 heap-env)
    (u16 depth)
    (u16 bcount)
    (u16 bmcount))


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
    (let* ((code (i-func-code func))
           (bcount (length (i-func-bindings func)))
           (bmcount (length (i-func-bindmap func)))
           (mem (make-bytevector
                  (+ (* 4 (length code)) ; code
                     (* 4 bcount) ; bindings
                     (* 2 bmcount))))) ; bindmap

      (define (make-dbg-sym cmd)
        (string->utf8
          (if (null? (cdddr cmd))
            "\x0;"
            (let ((sym (cadddr cmd)))
              (string-append
                (if (string? sym)
                  sym
                  (symbol->string sym))
                "\x0;")))))

      (define (assemble-main cur dbg stack-use stack-size offset)
        (if (null? cur)
          (values stack-size (reverse dbg))
          (let* ((cmd (car cur))
                 (nuse (+ stack-use (caddr cmd))))
            (bytevector-s16-native-set! mem offset (opcode (car cmd)))
            (bytevector-s16-native-set! mem (+ 2 offset) (cadr cmd))
            (assemble-main (cdr cur)
                           ;; Add debug symbols
                           (cons (make-dbg-sym cmd) dbg)
                           nuse
                           (max nuse stack-size)
                           (+ offset 4)))))

      (let* ((bindmap-offset
               (fold-left (lambda (offset bind)
                            (bytevector-u16-native-set! mem offset (car bind))
                            (bytevector-u16-native-set! mem (+ 2 offset) (cdr bind))
                            (+ offset 4))
                          0 (i-func-bindings func)))
             (code-offset
               (fold-left (lambda (offset bm)
                            (bytevector-u16-native-set! mem offset bm)
                            (+ 2 offset))
                          bindmap-offset (i-func-bindmap func))))
        (let-values (((res-size dbg) (assemble-main code '() 0 0 code-offset)))
          (values
            (make-fun-hdr (i-func-size func)    ; env size
                          (i-func-argc func)    ; argc
                          (bool->int (i-func-swallow func))
                          res-size              ; stack size
                          (length code)         ; op count
                          (bool->int (i-func-heap? func))  ; allocate env on heap?
                          (i-func-depth func)   ; depth
                          bcount                ; count of bindings
                          bmcount)              ; size of bindmap
            dbg mem)))))

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

      (set-port-position! asm-port (header-size))
      (let* ((header
               (make-header
                 (write-strings asm-port (ilr-imports root))
                 (write-strings asm-port (ilr-symbols root))
                 (write-strings asm-port (ilr-strings root))
                 (length (ilr-code root))
                 (ilr-entry-point root))))

        (fold-left (lambda (idx func)
                     (let-values (((hdr dbg code) (assemble-code func)))
                       (write-dbg dbg)
                       (put-bytevector asm-port hdr)
                       (put-bytevector asm-port code))
                     (+ 1 idx))
                   0 (ilr-code root))

        (set-port-position! asm-port 0)
        (put-bytevector asm-port header))

      (close-output-port asm-port)
      (close-output-port dbg-port)))
  )
