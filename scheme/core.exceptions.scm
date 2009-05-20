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

  (exception-handlers (cons default-exception
                           (exception-handlers)))

  (define (with-exception-handler new thunk)
    (let ((parent (exception-handlers)))
      (exception-handlers (cons new parent))
      (let ((res (thunk)))
        (exception-handlers parent)
        res)))

  (define (raise x)
    (let ((handlers (exception-handlers)))
      (exception-handlers (cdr handlers))
      ((car handlers) x)
      (__exit)))

  (define (raise-continuable x)
    (let ((handlers (exception-handlers)))
      (exception-handlers (cdr handlers))
      (let ((res ((car handlers) x)))
        (exception-handlers handlers)
        res)))

  (define-syntax assert
    (syntax-rules ()
      ((_ expr)
       (if (not expr)
           (raise '("Assertion failed:" expr))))))
  )
