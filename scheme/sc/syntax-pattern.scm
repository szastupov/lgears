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

(library (sc syntax-pattern)
  (export pattern-match pattern-bind pattern-bind-named)
  (import (rnrs base)
          (rnrs lists)
          (format)
          (sc syntax-core))

  (define (ellipsis-pair? pat)
    (and (pair? pat)
         (pair? (cdr pat))
         (eq? (cadr pat) '...)))

  (define (initial-merge pat xpr)
    (cond ((pair? pat)
           (cons (initial-merge (car pat) (car xpr))
                 (initial-merge (cdr pat) (cdr xpr))))
          ((null? pat) '())
          (else (list xpr))))

  (define (pattern-merge pat xpr1 xpr2)
    (cond ((null? xpr2) (initial-merge pat xpr1))
          ((pair? pat)
           (cons (pattern-merge (car pat) (car xpr1) (car xpr2))
                 (pattern-merge (cdr pat) (cdr xpr1) (cdr xpr2))))
          (else (cons xpr1 xpr2))))

  (define (pattern-match reserved xpr pat)
    (call/cc
     (lambda (return)
       (define (match-ellipsis xpr pat)
         (cond ((syntax-null? xpr) '())
               ((syntax-pair? xpr)
                (pattern-merge pat (match (syntax-car xpr) pat)
                               (match-ellipsis (syntax-cdr xpr) pat)))
               (else (return #f))))

       (define (match xpr pat)
         (cond ((ellipsis-pair? pat)
                (let ((elm (match-ellipsis xpr (car pat))))
                  (if (pair? (car pat))
                      elm
                      (list elm))))
               ((null? pat)
                (if (and (syntax-null? xpr)
                         (null? pat))
                    '()
                    (return #f)))
               ((pair? pat)
                (if (syntax-pair? xpr)
                    (cons (match (syntax-car xpr) (car pat))
                          (match (syntax-cdr xpr) (cdr pat)))
                    (return #f)))
               ((eq? pat '_) '())
               ((symbol? pat)
                (if (memq pat reserved)
                    (if (and (identifier? xpr)
                             (eq? pat (syntax-object-expr xpr)))
                        xpr
                        (return #f))
                    xpr))
               ((equal? pat (syntax-object-expr xpr)) '())
               (else (return #f))))

       (match xpr pat))))

  (define (pattern-bind xpr pat vars)
    (cond ((ellipsis-pair? pat)
           (append xpr vars))
          ((pair? pat)
           (pattern-bind (car xpr) (car pat)
                         (pattern-bind (cdr xpr) (cdr pat) vars)))
          ((eq? pat '_) vars)
          ((symbol? pat)
           (cons xpr vars))
          (else vars)))

  (define (bind-null-ellipsis pat vars)
    (if (and (pair? pat)
             (pair? (car pat)))
        (fold-left (lambda (prev x)
                     (bind-null-ellipsis x prev))
                   vars (car pat))
        (cons `(,pat ()) vars)))


  (define (bind-ellipsis-named reserved xpr pat vars)
    (cond ((null? xpr)
           (bind-null-ellipsis pat vars))
          ((pair? (car pat))
           (pattern-bind-named reserved
                               (car xpr)
                               (caar pat)
                               (pattern-bind-named reserved
                                                   (cdr xpr)
                                                   (cdar pat)
                                                   vars)))
          (else (cons (cons (car pat) (car xpr)) vars))))

  (define (pattern-bind-named reserved xpr pat vars)
    (cond ((ellipsis-pair? pat)
           (bind-ellipsis-named reserved xpr pat vars))
          ((pair? pat)
           (pattern-bind-named reserved
                               (car xpr)
                               (car pat)
                               (pattern-bind-named reserved
                                                   (cdr xpr)
                                                   (cdr pat)
                                                   vars)))
          ((eq? pat '_) vars)
          ((symbol? pat)
           (if (memq pat reserved)
               vars
               (cons (cons pat xpr)
                     vars)))
          (else vars)))

  )
