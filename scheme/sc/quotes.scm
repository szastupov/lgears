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

(library (sc quotes)
  (export trquote trquasiquote)
  (import (rnrs)
          (format))

  (define (self-eval? x)
    (or (number? x)
        (string? x)
        (char? x)
        (boolean? x)))

  ; Common transquote routine
  (define (trquote-common qv pfunc)
    (define (apply-pfunc x)
      (cond ((pair? x)
             (pfunc x))
            ((symbol? x)
             `(quote ,x))
            ((null? x)
             ''())
            (else x)))

    (cond ((list? qv)
           (cons 'list (map apply-pfunc qv)))
          ((null? qv)
           '())
          ((pair? qv)
           `(cons ,(apply-pfunc (car qv))
                  ,(trquote-common (cdr qv) pfunc)))
          (else (apply-pfunc qv))))

  ; Translate quotation to functions
  (define (trquote qv)
    (trquote-common qv trquote))

  ; Translate quoasiquotation to functions
  ; FIXME: implement splicing
  (define (trquasiquote qv)
    (trquote-common
      qv
      (lambda (head)
        (case (car head)
          ((unquote)
           (cadr head))
          ((unquote-splicing)
           (cadr head))
          (else
            (trquasiquote head))))))

  )
