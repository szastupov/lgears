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
(library (base)
  (export eqv? equal?)
  (import (coreforms)
          ($builtin)
          (exceptions)
          (core_sequence))

  ;; In context of lgears, eqv? and eq? is same
  (define eqv? eq?)

  (define (compare-pairs a b)
    (let loop ((a a)
               (b b))
      (cond ((and (null? a)
                  (null? b))
             #t)
            ((and (pair? a) (pair? b)
                  (eqv? (car a) (car b)))
             (loop (cdr a) (cdr b)))
            (else
             (eqv? a b)))))

  (define (compare-vectors a b)
    (if ($= (vector-length a)
           (vector-length b))
        (let ((len (vector-length a)))
          (let loop ((n 0))
            (cond (($= n len)
                   #t)
                  ((eqv? (vector-ref a n)
                         (vector-ref b n))
                   (loop ($+ 1 n)))
                  (else #f))))
        #f))

  (define (equal? a b)
    (cond ((and (string? a)
                (string? b))
           (string=? a b))
          ((and (pair? a)
                (pair? b))
           (compare-pairs a b))
          ((and (vector? a)
                (vector? b))
           (compare-vectors a b))
          (else
           (eqv? a b))))

;; String and character utilites

  (define (char-op op args)
    (apply op (map char->integer args)))

  (define (char=? . args)
    (char-op = args))

  (define (char<? . args)
    (char-op < args))

  (define (char>? . args)
    (char-op > args))

  (define (char<=? . args)
    (char-op <= args))

  (define (char>=? . args)
    (char-op >= args))

  (define (in-range c f t)
    (and (char<=? f c)
         (char>=? t c)))

  (define (make-subtrahend c)
    (cond ((in-range c #\0 #\9) 48)
          ((in-range c #\a #\f) 87)
          ((in-range c #\A #\F) 55)
          (else (error 'char->number "invalid character" c))))

  (define (char->number n)
    (- (char->integer n)
       (make-subtrahend n)))

  (define (string->num str base)
    (let ((len (- (string-length str) 1)))
      (define (ref idx)
        (char->number
          (string-ref str (- len idx))))
      (let loop ((n len)
                 (res 0))
        (if (= n 0)
          ($+ res (ref n))
          (loop ($- n 1)
                ($+ res ($* (ref n)
                          (expt base n))))))))

  (define call-with-current-continuation call/cc)

  (display "Base library loaded\n")
  )
