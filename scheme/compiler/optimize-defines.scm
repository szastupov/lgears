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

(library (compiler optimize-defines)
  (export cleanup-defines)
  (import (rnrs)
          (compiler analyze))

  (define (filter-defines body)
    (let loop ((body body))
      (if (null? body)
          '()
          (let ((node (car body)))
            (if (tagged? node 'define)
                (let ((bind (cadr node)))
                  (if (> (binding-ref-count (cdr bind)) 0)
                      (cons `(define ,(car bind)
                               ,(optimize-defines (caddr node)))
                            (loop (cdr body)))
                      (loop (cdr body))))
                (cons (optimize-defines node)
                      (loop (cdr body))))))))

  (define (optimize-defines node)
    (cond ((function? node)
           `(lambda ,(function-args node)
              ,@(filter-defines (function-body node))))
          ((self-eval? node) node)
          ((pair? node)
           (cons (optimize-defines (car node))
                 (optimize-defines (cdr node))))
          (else node)))

  (define (cleanup-defines node)
    ;node)
    (optimize-defines (analyze '() node)))
  )