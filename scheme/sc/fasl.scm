#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov
 |
 | This program is free software; you can redistribute it and/or
 | modify it under the terms of the GNU Lesser General Public
 | License as published by the Free Software Foundation; either
 | version 3 of the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 | Lesser General Public License for more details.
 |
 | You should have received a copy of the GNU Lesser General
 | Public Licens along with this program, if not; see
 | <http://www.gnu.org/licenses>.
 |#

(library (sc fasl)
  (export assemble
          make-ilr make-i-func make-static
          i-func-code-set! i-func-code print-ilr)
  (import (rnrs)
          (sc opcode)
          (cpacked)
          (debug)
          (format))

  (define-record-type static
    (fields name))

  (define-record-type ilr
    (fields consts code
            entry-point))

  (define-record-type i-func
    (fields (mutable code) size argc swallow
            heap? depth bindings bindmap))

  (define-cpacked
    header
    (u32 consts-size)
    (u16 fun-count)
    (u16 entry-point))

  (define-cpacked
    fun-hdr
    (u16 env-size)
    (u16 argc)
    (u16 swallow)
    (u16 op-count)
    (u16 heap-env)
    (u16 depth)
    (u16 bcount)
    (u16 bmcount))


  (define (print-ilr res)
    (display "ILR: \n")
    (format #t "Consts: ~s\n" (ilr-consts res))
    (format #t "Entry point: ~a\n" (ilr-entry-point res))
    (display "Code: \n")
    (for-each (lambda (x)
                (format #t "~s\n" (i-func-code x)))
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
         (if (null? (cddr cmd))
             "\x0;"
             (let ((sym (caddr cmd)))
               (string-append
                sym	"\x0;")))))

      (define (assemble-main cur dbg offset)
        (if (null? cur)
            (reverse dbg)
            (let* ((cmd (car cur)))
              (bytevector-s16-native-set! mem offset (opcode (car cmd)))
              (bytevector-s16-native-set! mem (+ 2 offset) (cadr cmd))
              (assemble-main (cdr cur)
                             ;; Add debug symbols
                             (cons (make-dbg-sym cmd) dbg)
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
        (let ((dbg (assemble-main code '() code-offset)))
          (values
           (make-fun-hdr (i-func-size func)    ; env size
                         (i-func-argc func)    ; argc
                         (bool->int (i-func-swallow func))
                         (length code)         ; op count
                         (bool->int (i-func-heap? func))  ; allocate env on heap?
                         (i-func-depth func)   ; depth
                         bcount                ; count of bindings
                         bmcount)              ; size of bindmap
           dbg mem)))))

  (define (number->u16 n)
    (let ((bv (make-bytevector 2)))
      (bytevector-u16-native-set! bv 0 n)
      bv))

  (define (number->s64bv n)
    (let ((bv (make-bytevector 8)))
      (bytevector-s64-native-set! bv 0 n)
      bv))

  ;; Convert string to bytevector for constants
  (define (string->cbv str)
    (let* ((utf8 (string->utf8 str))
           (len (+ 1 (bytevector-length utf8)))
           (bv (make-bytevector (+ 1 len))))
      (bytevector-u8-set! bv 0 len)
      (bytevector-u8-set! bv len 0) ; null-terminate string
      (bytevector-copy! utf8 0 bv 1 (- len 1))
      bv))

  (define (char->u16bv chr)
    (let ((bv (make-bytevector 2)))
      (bytevector-u16-native-set! bv 0 (char->integer chr))
      bv))

  (define (write-consts port consts)
    (define (put-type type)
      (put-u8 port (object-type type)))
    (let ((oldpos (port-position port)))
      (put-u8 port (length consts))
      (for-each
       (lambda (obj)
         (cond ((char? obj)
                (put-type 'OT_CHARACTER)
                (put-bytevector port (char->u16bv obj)))
               ((number? obj)
                (put-type 'OT_FIXNUM)
                (put-bytevector port (number->s64bv obj)))
               ((symbol? obj)
                (put-type 'OT_SYMBOL)
                (put-bytevector port
                                (string->cbv
                                 (symbol->string obj))))
               ((string? obj)
                (put-type 'OT_STRING)
                (put-bytevector port (string->cbv obj)))
               ((static? obj)
                (put-type 'OT_STATIC)
                (put-bytevector port (string->cbv (static-name obj))))
               (else (error 'write-consts "unknown" obj))))
       consts)
      (- (port-position port) oldpos)))

  (define (assemble root path)
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
               (write-consts asm-port (ilr-consts root))
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
