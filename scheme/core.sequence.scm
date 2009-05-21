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
(library (core.sequence)
  (export for-each map fold-left fold-right reverse append make-list
          vector vector? make-vector vector-ref vector-set!  vector->list
          vector-for-each vector-map make-vector string->list string-for-each
          string-append caaaar cdaaar cadaar cddaar caadar cdadar caddar cdddar
          caaadr cdaadr cadadr cddadr caaddr cdaddr cadddr cddddr caaar cdaar
          cadar cddar caadr cdadr caddr cdddr caar cdar cadr cddr)
  (import (core.forms)
          (core.exceptions))

  (define (caaaar x) (car (car (car (car x)))))
  (define (cdaaar x) (cdr (car (car (car x)))))
  (define (cadaar x) (car (cdr (car (car x)))))
  (define (cddaar x) (cdr (cdr (car (car x)))))
  (define (caadar x) (car (car (cdr (car x)))))
  (define (cdadar x) (cdr (car (cdr (car x)))))
  (define (caddar x) (car (cdr (cdr (car x)))))
  (define (cdddar x) (cdr (cdr (cdr (car x)))))
  (define (caaadr x) (car (car (car (cdr x)))))
  (define (cdaadr x) (cdr (car (car (cdr x)))))
  (define (cadadr x) (car (cdr (car (cdr x)))))
  (define (cddadr x) (cdr (cdr (car (cdr x)))))
  (define (caaddr x) (car (car (cdr (cdr x)))))
  (define (cdaddr x) (cdr (car (cdr (cdr x)))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))
  (define (cddddr x) (cdr (cdr (cdr (cdr x)))))
  (define (caaar x) (car (car (car x))))
  (define (cdaar x) (cdr (car (car x))))
  (define (cadar x) (car (cdr (car x))))
  (define (cddar x) (cdr (cdr (car x))))
  (define (caadr x) (car (car (cdr x))))
  (define (cdadr x) (cdr (car (cdr x))))
  (define (caddr x) (car (cdr (cdr x))))
  (define (cdddr x) (cdr (cdr (cdr x))))
  (define (caar x) (car (car x)))
  (define (cdar x) (cdr (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cddr x) (cdr (cdr x)))

 #|
  ;; Generator
  (define (cXr vars)
    (if (null? vars)
        'x
        (list (format "c~ar" (car vars))
              (cXr (cdr vars)))))

  (define (print-define res)
    (format #t "(define (c~ar x) ~a)\n"
            (apply string-append res)
            (cXr res)))

  (define (print-name res)
    (format #t "c~ar " (apply string-append res)))

  (define (gen pf n)
    (let gen ((n n)
              (res '()))
      (if (= n 0)
          (pf res)
          (begin
            (gen (- n 1) (cons "a" res))
            (gen (- n 1) (cons "d" res))))))

  (define (gencXr pf)
    (do ((i 4 (- i 1)))
        ((= i 1))
      (gen pf i)))

  (gencXr print-name)
  (gencXr print-define)
  |#

  ;; List utilites
  (define (for-each proc lst1 . lst2)
    (define (for-each-1 lst)
      (if (null? lst)
          (void)
          (begin
            (proc (car lst))
            (for-each-1 (cdr lst)))))

    (define (for-each-n lst)
      (if (null? (car lst))
          (void)
          (begin
            (apply proc (map car lst))
            (for-each-n (map cdr lst)))))

    ;; TODO chech that length of lists is same
    (assert (procedure? proc))
    (if (null? lst2)
        (for-each-1 lst1)
        (for-each-n (cons lst1 lst2))))

  (define (map proc lst1 . lst2)
    (define (map-1 lst)
      (if (null? lst)
          '()
          (begin
            (cons (proc (car lst))
                  (map-1 (cdr lst))))))

    (define (map-n lst)
      (if (null? (car lst))
          '()
          (cons (apply proc (map car lst))
                (map-n (map cdr lst)))))

    (assert (procedure? proc))
    (if (null? lst2)
        (map-1 lst1)
        (map-n (cons lst1 lst2))))

  (define (fold-left proc init lst1 . lst2)
    (define (fold-left-1 res lst)
      (if (null? lst)
          res
          (fold-left-1 (proc res (car lst)) (cdr lst))))

    (define (fold-left-n res lst)
      (if (null? (car lst))
          res
          (fold-left-n (apply proc (cons res (map car lst)))
                       (map cdr lst))))

    (assert (procedure? proc))
    (if (null? lst2)
        (fold-left-1 init lst1)
        (fold-left-n init (cons lst1 lst2))))

  (define (fold-right proc init lst1 . lst2)
    (define (fold-right-1 lst)
      (if (null? lst)
          init
          (proc (car lst) (fold-right-1 (cdr lst)))))

    (define (fold-right-n lst)
      (if (null? (car lst))
          init
          (apply proc (append (map car lst) (list (fold-right-n (map cdr lst)))))))

    (assert (procedure? proc))
    (if (null? lst2)
        (fold-right-1 lst1)
        (fold-right-n (cons lst1 lst2))))


  (define (reverse lst)
    (fold-left (lambda (x y)
                 (cons y x))
               '() lst))

  (define (append2 b a)
    (fold-right cons b a))

  (define (append . args)
    (if (null? args)
        '()
        (let ((rev (reverse args)))
          (fold-left append2 (car rev) (cdr rev)))))

  (define (make-list count . args)
    ($make-list count (if (null? args)
                          #f
                          (car args))))

  ;; Vector utilites

  (define (vector . args)
    (apply make-struct (cons 'vector args)))

  (define (vector? obj)
    (and (struct? obj)
         (eq? (struct-type obj) 'vector)))

  (define (vector-length obj)
    (assert (vector? obj))
    (struct-size obj))

  (define (vector-ref obj idx)
    (assert (vector? obj))
    (struct-ref obj idx))

  (define (vector-set! obj idx val)
    (assert (vector? obj))
    (struct-set! obj idx val))

  (define (make-vector size . args)
    (alloc-struct 'vector
                  size
                  (if (null? args)
                      (void)
                      (car args))))

  (define (vector->list obj)
    (assert (vector? obj))
    (struct->list obj))

  (define (vec-for-each-1 ref len proc vec)
    (let loop ((n 0))
      (if ($= n (len vec))
          (void)
          (begin
            (proc (ref vec n))
            (loop ($+ 1 n))))))

  ;; Note that for vector-for-each/vector-map we firstly check right
  ;; type and pass struct-* functions in order to reduce checks
  (define (vector-for-each-1 proc vec)
    (assert (vector? vec))
    (vec-for-each-1 struct-ref struct-size proc vec))

  (define vector-for-each vector-for-each-1)

  (define (vector-map-1 proc vec)
    (let ((res (make-vector (vector-length vec))))
      (let loop ((n 0))
        (if ($= n (struct-size vec))
            res
            (begin
              (struct-set! res n (proc (struct-ref vec n)))
              (loop ($+ 1 n)))))))

  (define vector-map vector-map-1)

  (define (string->list str)
    (let loop ((pos ($- (string-length str) 1))
               (res '()))
      (if ($- pos 0)
          (cons (string-ref str pos) res)
          (loop ($- pos 1)
                (cons (string-ref str pos) res)))))

  (define (string-for-each-1 proc vec)
    (vec-for-each-1 string-ref string-length proc vec))

  (define string-for-each string-for-each-1)

  (define (string-append . args)
    (fold-left string-concat "" args))
  )
