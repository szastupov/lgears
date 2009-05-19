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

(library (compiler source-optimizer)
  (export optimize-source)
  (import (rnrs)
          (format)
          (compiler analyze)
          (compiler primitives-info))

  (define (side-effects-free? node)
    (or (self-eval? node)
        (and (pair? node)
             (symbol? (car node))
             (cond ((assq (car node) primitives-builtin)
                    => (lambda (res)
                         (not (or (cadr res) (caddr res)))))
                   ((memq (car node) primitives-operators)
                    #t)
                   (else #f)))))

  (define (optimize-body body)
    (let loop ((cur body))
      (if (null? cur)
          '()
          (let ((node (car cur))
                (next (cdr cur)))
            (cond ((tagged? node 'define)
                   (let ((bind (cadr node)))
                     (if (> (binding-ref-count (cdr bind)) 0)
                         (cons `(define ,(car bind)
                                  ,(optimize (caddr node)))
                               (loop next))
                         (loop next))))
                  ((and (not (null? next))
                        (side-effects-free? node))
                   (loop next))
                  (else
                   (cons (optimize node)
                         (loop next))))))))

  (define (lambda-call? node)
    (and (pair? node)
         (function? (car node))))

  (define (may-inline-lambda? node)
    (and (exists self-eval? (cdr node))
         (not (null? (function-args (car node))))))

  (define (inline-binding! func name value)
    (cond ((assq name (function-bindings func))
           => (lambda (bp)
                 (binding-value-set! (cdr bp) value)))
          (else (error 'inline-binding! "wtf?" name))))

  (define (inline-lambda! func args)
    (let loop ((rval (function-args func))
               (sval args)
               (rused '())
               (sused '()))
      (cond ((null? rval)
             (values (reverse rused)
                     (reverse sused)))
            ((self-eval? (car sval))
             (inline-binding! func (car rval) (optimize (car sval)))
             (loop (cdr rval) (cdr sval) rused sused))
            (else
             (loop (cdr rval)
                   (cdr sval)
                   (cons (car rval) rused)
                   (cons (optimize (car sval)) sused))))))

  (define (optimize-lambda-call node)
    (let ((func (car node)))
      (if (may-inline-lambda? node)
          (let-values (((rused sused)
                        (inline-lambda! func (cdr node))))
            `((lambda ,rused
                ,@(optimize-body (function-body func)))
              ,@sused))
          (map optimize node))))

  (define (optimize node)
    (cond ((function? node)
           `(lambda ,(function-args node)
              ,@(optimize-body (function-body node))))
          ;; ((lambda-call? node)
          ;;  (optimize-lambda-call node))
          ((reference? node)
           (binding-value (reference-bind node)))
          ((self-eval? node) node)
          ((pair? node)
           (map optimize node))
          (else node)))

  (define (optimize-source node)
    (optimize (analyze '() node)))
  )
