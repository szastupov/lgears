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

  (define (optimize node)
    (cond ((function? node)
           `(lambda ,(function-args node)
              ,@(optimize-body (function-body node))))
          ((self-eval? node) node)
          ((pair? node)
           (cons (optimize (car node))
                 (optimize (cdr node))))
          (else node)))

  (define (optimize-source node)
    (optimize (analyze '() node)))
  )
