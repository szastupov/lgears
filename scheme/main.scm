#!/usr/bin/env ypsilon
#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov
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

(add-library-path ".")

(import (rnrs)
        (compiler)
        (format)
        (config))

(define (full-compile file)
  (let ((assm (format "~a/~a.o" cache-path file)))
    (compile-file file assm)))

(let* ((argv (command-line))
       (argc (length argv)))
  (cond ((< argc 2)
         (format #t "Usage: ~a [-E -C -A] file.scm\n" (car argv))
         (exit 1))
        ((= argc 3)
         (let* ((flag (cadr argv))
                (file (caddr argv)))
           (cond ((equal? flag "-E")
                  (format #t ";;; Expanded from ~a\n" file)
                  (pretty-print (expand-file file)))

                 ((equal? flag "-C")
                  (format #t ";;; CPS-converted from ~a\n" file)
                  (pretty-print (cps-convert (expand-file file))))

                 ((equal? flag "-A")
                  (dump-ilr file))

                 (else
                   (error 'main "unknown option" (cadr argv))))))
        (else
          (full-compile (cadr argv)))))
(newline)
