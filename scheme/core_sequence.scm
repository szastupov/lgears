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
(library (core_sequence)
  (export for-each map fold-left fold-right reverse append make-list
          vector-for-each vector-map make-vector string->list
          string-for-each string-append)
  (import ($builtin)
          (coreforms))

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

  (define (vec-for-each-1 ref len proc vec)
    (let loop ((n 0))
      (if ($= n (len vec))
          (void)
          (begin
            (proc (ref vec n))
            (loop ($+ 1 n))))))

  (define (vector-for-each-1 proc vec)
    (vec-for-each-1 vector-ref vector-length proc vec))

  (define vector-for-each vector-for-each-1)

  (define (vector-map-1 proc vec)
    (let ((res (make-vector (vector-length vec))))
      (let loop ((n 0))
        (if ($= n (vector-length vec))
            res
            (begin
              (vector-set! res n (proc (vector-ref vec n)))
              (loop ($+ 1 n)))))))

  (define vector-map vector-map-1)

  ;; Rewrite it with case-lambda
  (define (make-vector size . args)
    ($make-vector size (if (null? args)
                           (void)
                           (car args))))

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