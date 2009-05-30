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

(library (core.strings)
  (export list->string string string->list string-append string-split)
  (import (core.forms)
          (core.sequence)
          (core.arithmetic))

  (define (list->string lst)
    (let* ((slen (length lst))
           (sres (make-string slen #\space)))
      (let loop ((pos 0)
                 (chars lst))
        (if (null? chars)
            sres
            (begin
              (string-set! sres pos (car chars))
              (loop (+ pos 1) (cdr chars)))))))

  (define (string . args)
    (list->string args))

  (define (string->list str)
    (let loop ((pos (- (string-length str) 1))
               (res '()))
      (if (= pos 0)
          (cons (string-ref str pos) res)
          (loop (- pos 1)
                (cons (string-ref str pos) res)))))

  (define (sum-strings strings)
    (fold-left
     (lambda (len s)
       (+ len (string-length s)))
     0 strings))

  (define (string-append . args)
    (let* ((tlen (sum-strings args))
           (res (make-string tlen #\space)))
      (let loop ((args args)
                 (pos 0))
        (if (null? args)
            res
            (begin
              (string-copy! res pos (car args))
              (loop (cdr args)
                    (+ pos (string-length (car args)))))))))

  (define (string-split str sep)
    (define (match? n)
      (memq (string-ref str n) sep))
    (let ((len (string-length str)))
      (let loop ((last 0)
                 (pos 0))
        (define (ss l)
          (substring str last l))
        (cond ((= pos (- len 1))
               (if (match? pos)
                   (if (= (- len 1) last)
                       '()
                       (list (ss (if (match? pos)
                                     (- len 1)
                                     len))))
                   (list (ss len))))
              ((match? pos)
               (if (= last pos)
                   (loop (+ 1 last) (+ 1 pos))
                   (cons (ss pos)
                         (loop (+ 1 pos) (+ 1 pos)))))
              (else
               (loop last (+ 1 pos)))))))
  )