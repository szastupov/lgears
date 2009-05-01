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

(library (compiler)
  (export compile-file expand-file cps-convert dump-ilr)
  (import (rnrs)
          (sc expand)
          (sc cps)
          (sc compiler)
          (sc fasl)
          (only (core) pretty-print)) ; This works only for ypsilon

  (define (compile-ilr-file in)
    (start-compile
      (cps-convert
       (expand-file in))))

  (define (compile-file in out)
    (assemble (compile-ilr-file in)
              out))

  (define (dump-ilr path)
    (print-ilr (compile-ilr-file path)))

  )
