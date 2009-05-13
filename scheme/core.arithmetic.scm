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
  (export + f-add * f-mul - f-sub / f-div mod < f-< > f-> = f-=)
  (import (core.forms)
          (core.sequence))

  (define (f-add . args)
    (fold-left (lambda (x y)
                 ($+ x y))
               0 args))

  (define (f-mul . args)
    (fold-left (lambda (x y)
                 ($* x y))
               1 args))

  (define (f-sub init . args)
    (if (null? args)
        ($- 0 init)
        (fold-left (lambda (x y)
                     ($- x y))
                   init args)))

  (define (f-div init . args)
    (if (null? args)
        ($/ 1 init)
        (fold-left (lambda (x y)
                     ($/ x y))
                   init args)))

  (define-syntax +
    (lambda (x)
      (syntax-case x ()
        ((_) #'0)
        ((_ a) #'a)
        ((_ a b) #'($+ a b))
        ((_ a b c ...) #'(+ ($+ a b) c ...))
        (_ #'f-add))))

  (define-syntax *
    (lambda (x)
      (syntax-case x ()
        ((_) #'1)
        ((_ a) #'a)
        ((_ a b) #'($* a b))
        ((_ a b c ...) #'(* ($* a b) c ...))
        (_ #'f-mul))))

  (define-syntax -
    (lambda (x)
      (syntax-case x ()
        ((_) (syntax-error x "required at least 1 argument"))
        ((_ a) #'($- 0 a))
        ((_ a b) #'($- a b))
        ((_ a b c ...) #'(- ($- a b) c ...))
        (_ #'f-sub))))

  (define-syntax /
    (lambda (x)
      (syntax-case x ()
        ((_) (syntax-error x "required at least 1 argument"))
        ((_ a) #'($/ 1 a))
        ((_ a b) #'($/ a b))
        ((_ a b c ...) #'(/ ($/ a b) c ...))
        (_ #'f-div))))

  (define-syntax mod
    (lambda (x)
      (syntax-case x ()
        ((_ a b) #'($% a b))
        (a (if (identifier? #'a)
               #'(lambda (x y)
                   ($% x y))
               (syntax-error x "invalid syntax"))))))

  (define (cmp proc init args)
    (let loop ((a init)
               (arg args))
      (cond ((null? arg) #t)
            ((proc a (car arg))
             (loop (car arg) (cdr arg)))
            (else #f))))

  (define (f-< init . args)
    (cmp (lambda (x y)
           ($< x y))
         init args))

  (define-syntax <
    (lambda (x)
      (syntax-case x ()
        ((_) (syntax-error x "required at least 1 argument"))
        ((_ a) '#t)
        ((_ a b) #'($< a b))
        ((_ a b c ...) #'(and ($< a b) (< c ...)))
        (_ #'f-<))))

  (define (f-> init . args)
    (cmp (lambda (x y)
           ($> x y))
         init args))

  (define-syntax >
    (lambda (x)
      (syntax-case x ()
        ((_) (syntax-error x "required at least 1 argument"))
        ((_ a) '#t)
        ((_ a b) #'($> a b))
        ((_ a b c ...) #'(and ($> a b) (> c ...)))
        (_ #'f->))))

  (define (f-= init . args)
    (cmp (lambda (x y)
           ($= x y))
         init args))

  (define-syntax =
    (lambda (x)
      (syntax-case x ()
        ((_) (syntax-error x "required at least 1 argument"))
        ((_ a) '#t)
        ((_ a b) #'($= a b))
        ((_ a b c ...) #'(and ($= a b) (= c ...)))
        (_ #'f-=))))

  )
