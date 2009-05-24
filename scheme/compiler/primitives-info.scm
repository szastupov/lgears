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

(library (compiler primitives-info)
  (export primitives-builtin primitives-operators operation?)
  (import (rnrs base)
          (rnrs lists))

  (define primitives-operators
    '($cons $car $cdr $- $+ $* $/ $% $< $> $= $! $eq?))

  (define (operation? node)
    (and (pair? node)
         (symbol? (car node))
         (memq (car node) primitives-operators)))

  ;; The table of builtin functions which also describe functions safity
  ;; (name side-effect? change-cont?)
  (define primitives-builtin
    '((display #t #f)
      (__exit #t #f)
      (call/cc #t #t)
      (apply #f #t)
      (list #f #f)
      ($make-list #f #f)
      (list? #f #f)
      (length #f #f)
      (void #f #f)
      (eof-object #f #f)
      (eof-object? #f #f)
      (char->integer #f #f)
      (integer->char #f #f)
      (procedure? #f #f)
      (boolean? #f #f)
      (null? #f #f)
      (char? #f #f)
      (number? #f #f)
      (pair? #f #f)
      (symbol? #f #f)
      (make-struct #f #f)
      (alloc-struct #f #f)
      (struct? #f #f)
      (struct-size #f #f)
      (struct-ref #f #f)
      (struct-set! #t #f)
      (struct-type #f #f)
      (struct->list #f #f)
      (symbol->string #f #f)
      (string-ref #f #f)
      (string-length #f #f)
      (string? #f #f)
      (string=? #f #f)
      (string-concat #f #f)
      (make-bytevector #f #f)
      (bytevector? #f #f)
      (bytevector-u8-set! #t #f)
      (fd-open #t #f)
      (fd-close #t #f)
      (fd-seek #t #f)
      (fd-write #t #f)
      (fs-stat #f #f)
      (fs-remove #t #f)
      (fs-getcwd #f #f)
      (fs-opendir #t #f)
      (fs-readdir #t #f)
      (fs-closedir #t #f)
      (load-library #t #t)
      (library-cache #t #f)
      (load-so #t #f)
      (make-foreign #f #f)
      (exception-handlers #t #f)))

  )
