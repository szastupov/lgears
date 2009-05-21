#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
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

;;;
;;; TODO make it more generic
;;;

(library (core.arithmetic)
  (export + * - / mod < > = __cmp-proc __add/mul_proc __sub/div_proc)
  (import (core.forms)
          (core.sequence))

  (define (__add/mul_proc init proc)
    (lambda args
      (fold-left proc init args)))

  (define (__sub/div_proc init proc)
    (lambda (first . rest)
      (fold-left proc init (cons first rest))))

  (define-syntax add/mul
    (lambda (x)
      (syntax-case x ()
        ((_ op proc init)
         #'(lambda (y)
             (syntax-case y ()
               ((_) #'init)
               ((_ a) #'a)
               ((_ a b) #'(op a b))
               ((mac a b c ...) #'(mac (op a b) c ...))
               (id (identifier? #'id)
                   #'(__add/mul_proc init (lambda (x y) (op x y))))))))))

  (define-syntax +
    (add/mul $+ f-add 0))

  (define-syntax *
    (add/mul $* f-mul 1))

  (define-syntax sub/div
    (lambda (x)
      (syntax-case x ()
        ((_ op proc init)
         #'(lambda (y)
             (syntax-case y ()
               ((_) (syntax-error y "required at least 1 argument"))
               ((_ a) #'(op init a))
               ((_ a b) #'(op a b))
               ((mac a b c ...) #'(mac (op a b) c ...))
               (id (identifier? #'id)
                   #'(__sub/div_proc init (lambda (x y) (op x y))))))))))

  (define-syntax -
    (sub/div $- f-sub 0))

  (define-syntax /
    (sub/div $/ f-div 1))

  (define-syntax mod
    (op-binary $%))

  (define (__cmp-proc proc)
    (lambda (init . args)
      (let loop ((a init)
                 (arg args))
        (cond ((null? arg) #t)
              ((proc a (car arg))
               (loop (car arg) (cdr arg)))
              (else #f)))))

  (define-syntax cmp-op
    (lambda (x)
      (syntax-case x ()
        ((_ op)
         #'(lambda (y)
             (syntax-case y ()
               ((_) (syntax-error y "required at least 1 argument"))
               ((_ a) #'#t)
               ((_ a b) #'(op a b))
               ((mac a b c ...) #'(and (op a b) (mac c ...)))
               (id (identifier? #'id)
                   #'(__cmp-proc (lambda (x y) (op x y))))))))))

  (define-syntax <
    (cmp-op $<))

  (define-syntax >
    (cmp-op $>))

  (define-syntax =
    (cmp-op $=))

  )
