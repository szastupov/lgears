#!/usr/bin/env ypsilon
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
(add-library-path ".")

(import (rnrs)
        (compiler)
        (format))

(define (full-compile file)
  (let ((assm (string-append "compiled/" file ".o")))
    (if (not (file-exists? "compiled"))
      (create-directory "compiled"))
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
