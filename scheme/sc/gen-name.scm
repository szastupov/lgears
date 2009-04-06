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
