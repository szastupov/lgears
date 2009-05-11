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
(library (core.exceptions)
  (export with-exception-handler raise raise-continuable assert)
  (import (core.forms))

  (define (default-exception obj)
    (display "Unhandled excetion with: ")
    (display obj)
    (display "\n")
    (__exit))

  (define exception-handler (list default-exception))

  (define (with-exception-handler new thunk)
    (let ((parent exception-handler))
      (set! exception-handler (cons new parent))
      (let ((res (thunk)))
        (set! exception-handler parent)
        res)))

  (define (raise x)
    (let ((handlers exception-handler))
      (set! exception-handler (cdr handlers))
      ((car handlers) x)
      (__exit)))

  (define (raise-continuable x)
    (let ((handlers exception-handler))
      (set! exception-handler (cdr handlers))
      (let ((res ((car handlers) x)))
        (set! exception-handler handlers)
        res)))

  (define-syntax assert
    (syntax-rules ()
      ((_ expr)
       (if (not expr)
           (raise 'expr)))))

  )
