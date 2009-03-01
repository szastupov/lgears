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
 |
 | This format is incomplete and mostly works like
 | srfi-28 (http://srfi.schemers.org/srfi-28/srfi-28.html),
 | additional features will be added on demand
 |#

(library (format)
  (export format)
  (import (rnrs base)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs control))

  (define (format format-string . objects)
    (call-with-string-output-port
      (lambda (port)
        (let loop ((fmt (string->list format-string))
                   (objects objects))

          (define (assert-objects)
            (if (null? objects)
              (error 'format "No value for escape sequence")))

          (define (print-numeric radix)
            (assert-objects)
            (put-string port (number->string (car objects) radix))
            (loop (cddr fmt) (cdr objects)))

          (unless (null? fmt)
            (if (char=? (car fmt) #\~)
              (begin
                (if (null? (cdr fmt))
                  (error 'format "Incomplete escape sequence"))
                (case (cadr fmt)
                  ((#\a)
                   (assert-objects)
                   (display (car objects) port)
                   (loop (cddr fmt) (cdr objects)))
                  ((#\s)
                   (assert-objects)
                   (write (car objects) port)
                   (loop (cddr fmt) (cdr objects)))
                  ((#\x)
                   (print-numeric 16))
                  ((#\b)
                   (print-numeric 2))
                  ((#\o)
                   (print-numeric 8))
                  ((#\d)
                   (print-numeric 10))
                  ((#\%)
                   (newline port)
                   (loop (cddr fmt) objects))
                  ((#\~)
                   (put-char port #\~)
                   (loop (cddr fmt) objects))
                  (else
                    (error 'format "Unknown format option" (cadr fmt)))))
              (begin
                (put-char port (car fmt))
                (loop (cdr fmt) objects))))))))

;(display (format "~a ~s ~x ~b ~o ~d" "foo" "bar" 60 60 60 60))

)
