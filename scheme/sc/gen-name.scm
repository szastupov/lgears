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

(library (sc gen-name)
  (export gen-name)
  (import (rnrs)
          (format))

  (define last-name 0)

  (define (gen-name-impl src)
    (let ((res (string-append src (number->string last-name 16))))
      (set! last-name (+ last-name 1))
      (string->symbol res)))

  (define gen-name
    (case-lambda
      ((x) (gen-name-impl (format "~a." x)))
      (() (gen-name-impl "_var."))))
  )
