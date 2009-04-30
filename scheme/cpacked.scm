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

(library (cpacked)
  (export define-cpacked)
  (import (rnrs))

  (define-syntax define-cpacked
    (lambda (stx)
      (define (s+ . args)
        (string->symbol
          (apply string-append args)))

      (define (type->bvproc type proc)
        (string->symbol
          (string-append "bytevector-" (symbol->string type) "-" proc)))

      (define (type->size type)
        (let ((str (symbol->string type)))
          (/ (string->number
               (substring str 1 (string-length str)))
             8)))

      (define (parse-type field)
        (let ((type (car field)))
          (case type
            ((u8 s8)
             (list (type->bvproc type "ref")
                   (type->bvproc type "set!")
                   (type->size type)))
            ((u16 s16 u32 s32 u64 s64)
             (list (type->bvproc type "native-ref")
                   (type->bvproc type "native-set!")
                   (type->size type)))
            (else (error 'parse-type "Wrong type description" type)))))

      (define (calc-offsets types)
        (let loop ((offsets '(0))
                   (type types))
          (if (null? type)
            (values (reverse (cdr offsets)) (car offsets))
            (loop (cons (+ (caddar type)
                           (car offsets))
                        offsets)
                  (cdr type)))))

      (syntax-case stx ()
        ((_ name . fields)
         (let* ((struct-name (symbol->string
                               (syntax->datum #'name)))
                (types (map parse-type (syntax->datum #'fields)))
                (names (map (lambda (n)
                              (symbol->string (cadr n))) (syntax->datum #'fields))))
           (let-values (((offsets size) (calc-offsets types)))
             #`(begin
                 (define (#,(datum->syntax #'name (s+ struct-name "-size")))
                   #,size)
                 #,@(map (lambda (name type offset)
                           (datum->syntax 
                             #'name 
                             `(define (,(s+ struct-name "-" name) st)
                                (,(car type) st ,offset))))
                         names types offsets)
                 #,@(map (lambda (name type offset)
                           (datum->syntax
                             #'name
                           `(define (,(s+ struct-name "-" name "-set!") st val)
                               (,(cadr type) st ,offset val))))
                         names types offsets)

                 (define setters
                   (vector
                     #,@(map (lambda (name)
                               (datum->syntax
                                 #'name
                               `(lambda (vec val)
                                   (,(s+ struct-name "-" name "-set!") vec val))))
                             names)))

                 (define (#,(datum->syntax #'name (s+ "make-" struct-name)) . args)
                   (let ((bv (make-bytevector #,size)))
                     (fold-left (lambda (idx arg)
                                  ((vector-ref setters idx) bv arg)
                                  (+ idx 1))
                                0 args)
                     bv)))))))))
  )
