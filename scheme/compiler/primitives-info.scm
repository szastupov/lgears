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
    '($cons $car $cdr $- $+ $* $/ $% $< $> $= $! $eq? $type-test $and
            $ior $xor $not))

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
      (length #f #f)
      (void #f #f)
      (eof-object #f #f)
      (char->integer #f #f)
      (integer->char #f #f)
      (make-struct #f #f)
      (alloc-struct #f #f)
      (struct-size #f #f)
      (struct-ref #f #f)
      (struct-set! #t #f)
      (struct-type #f #f)
      (struct->list #f #f)
      (symbol->string #f #f)
      (string-ref #f #f)
      (string-set! #t #f)
      (string-length #f #f)
      (string? #f #f)
      (string=? #f #f)
      (string-concat #f #f)
      (substring #f #f)
      (string-copy #f #f)
      (string-copy! #t #f)
      (make-string #f #f)
      (make-bytevector #f #f)
      (bytevector-u8-set! #t #f)
      (load-library #t #t)
      (library-cache #t #f)
      (exception-handlers #t #f)))

  )
