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
  (export primitives-builtin)
  (import (rnrs base))

  ;; The table of builtin functions which also describe functions safity
  ;; (name side-effect? change-cont?)
  (define primitives-builtin
    '((display #t #f)
      (__exit #t #f)
      (call/cc #t #t)
      (apply #f #t)
      (cons #f #f)
      (list #f #f)
      ($make-list #f #f)
      (list? #f #f)
      (length #f #f)
      (car #f #f)
      (cdr #f #f)
      (void #f #f)
      (char->integer #f #f)
      (integer->char #f #f)
      (eq? #f #f)
      (procedure? #f #f)
      (boolean? #f #f)
      (null? #f #f)
      (char? #f #f)
      (number? #f #f)
      (pair? #f #f)
      (symbol? #f #f)
      ($+ #f #f)
      ($- #f #f)
      ($* #f #f)
      ($/ #f #f)
      (mod #f #f)
      ($= #f #f)
      ($< #f #f)
      ($> #f #f)
      (fxior #f #f)
      (vector #f #f)
      (vector? #f #f)
      (vector-length #f #f)
      (vector-set! #t #f)
      (vector-ref #f #f)
      (vector->list #f #f)
      ($make-vector #f #f)
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
      (load-library #t #t)
      (library-cache #t #f)))

  )