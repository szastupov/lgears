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

(library (sc syntax-core)
  (export syntax-object?
          make-syntax-object
          syntax-object-wrap
          syntax-object-expr
          make-mark
          mark?
          label?
          make-label
          make-subst
          subst?
          subst-sym
          subst-mark*
          subst-label
          make-binding
          binding-type
          binding-value
          strip
          syntax-error
          identifier?
          free-identifier?
          syntax-pair?
          syntax-car
          syntax-cdr
          syntax-cadr
          syntax-cddr
          syntax-caddr
          syntax-null?
          syntax->list
          syntax->pair
          syntax-length
          top-marked?
          top-mark
          self-evaluating?
          wrap-marks
          same-marks?
          extend-wrap
          join-wraps
          add-mark
          extend-env
          id-binding
          id-label
          label-binding
          )
  (import (except (rnrs) identifier?)
          (rnrs eval)
          (format)
          (define-structure)
          (sc gen-name))

  (define-structure syntax-object expr wrap)

  (define-structure mark)

  (define-structure subst sym mark* label)

  (define-structure label)

  (define make-binding cons)
  (define binding-type car)
  (define binding-value cdr)

  (define (strip x)
    (cond ((syntax-object? x)
           (strip (syntax-object-expr x)))
          ((pair? x)
           (let ((a (strip (car x)))
                 (d (strip (cdr x))))
             (if (and (eq? a (car x)) (eq? d (cdr x)))
                 x
                 (cons a d))))
          (else x)))

  (define (syntax-error what msg)
    (error (strip what) msg))

  (define (identifier? x)
    (and (syntax-object? x)
         (symbol? (syntax-object-expr x))))

  (define (free-identifier? x y)
    (eq? (id-label x) (id-label y)))

  (define (syntax-pair? x)
    (and (syntax-object? x)
         (pair? (syntax-object-expr x))))

  (define (syntax-car x)
    (extend-wrap
     (syntax-object-wrap x)
     (car (syntax-object-expr x))))

  (define (syntax-cdr x)
    (extend-wrap
     (syntax-object-wrap x)
     (cdr (syntax-object-expr x))))

  (define (syntax-cadr x)
    (syntax-car (syntax-cdr x)))

  (define (syntax-cddr x)
    (syntax-cdr (syntax-cdr x)))

  (define (syntax-caddr x)
    (syntax-car (syntax-cddr x)))

  (define (syntax-null? x)
    (and (syntax-object? x)
         (null? (syntax-object-expr x))))

  (define (syntax->list x)
    (if (syntax-null? x)
        '()
        (cons (syntax-car x)
              (syntax->list (syntax-cdr x)))))

  ;; Same as syntax->list but convert to improper list
  (define (syntax->pair x)
    (cond ((syntax-null? x)
          '())
          ((syntax-pair? x)
           (cons (syntax-car x)
                 (syntax->pair (syntax-cdr x))))
          (else x)))

  (define (syntax-length x)
    (length (syntax->list x)))

  (define top-mark (make-mark))

  (define (top-marked? wrap)
    (and (not (null? wrap))
         (or (eq? (car wrap) top-mark)
             (top-marked? (cdr wrap)))))
  
  (define (self-evaluating? x)
    (not (or (pair? x)
             (syntax-object? x))))

  (define (wrap-marks wrap)
    (if (null? wrap)
      '()
      (let ((w0 (car wrap)))
        (if (mark? w0)
          (cons w0 (wrap-marks (cdr wrap)))
          (wrap-marks (cdr wrap))))))

  (define (same-marks? m1* m2*)
    (if (null? m1*)
        (null? m2*)
        (and (not (null? m2*))
             (eq? (car m1*) (car m2*))
             (same-marks? (cdr m1*) (cdr m2*)))))

  (define (extend-wrap wrap x)
    (if (syntax-object? x)
        (make-syntax-object
         (syntax-object-expr x)
         (join-wraps wrap (syntax-object-wrap x)))
        (make-syntax-object x wrap)))

  (define (join-wraps wrap1 wrap2)
    (cond ((null? wrap1) wrap2)
          ((null? wrap2) wrap1)
          (else
           (let loop ((w (car wrap1))
                      (w* (cdr wrap1)))
             (if (null? w*)
                 (if (and (mark? w)
                          (eq? (car wrap2) w))
                     (cdr wrap2)
                     (cons w wrap2))
                 (cons w (loop (car w*) (cdr w*))))))))

  (define (add-mark mark x)
    (extend-wrap (list mark) x))

  (define (extend-env label binding env)
    (cons (cons label binding) env))

  (define (id-binding id r)
    (cond ((id-label id)
           => (lambda (label)
                (label-binding id label r)))
          (else #f)))

  (define (id-label id)
    (let ((sym (syntax-object-expr id))
          (wrap (syntax-object-wrap id)))
      (let search ((wrap wrap)
                   (mark* (wrap-marks wrap)))
        (if (null? wrap)
            #f
            (let ((w0 (car wrap)))
              (if (mark? w0)
                  (search (cdr wrap) (cdr mark*))
                  (if (and (eq? (subst-sym w0) sym)
                           (same-marks? (subst-mark* w0) mark*))
                      (subst-label w0)
                      (search (cdr wrap) mark*))))))))

  (define (label-binding id label r)
    (let ((a (assq label r)))
      (if a (cdr a)
          (syntax-error id "displaced lexical"))))

  )