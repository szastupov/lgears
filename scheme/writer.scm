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

(define (write-pair writer pair port)
  (put-char port #\()
  (let loop ((pair pair))
    (writer (car pair) port)
    (cond ((pair? (cdr pair))
           (put-char port #\space)
           (loop (cdr pair)))
          ((null? (cdr pair)) #t)
          (else
            (put-string port " . ")
            (writer (cdr pair) port))))
  (put-char port #\)))

(define (write-vector writer vec port)
  (put-char port #\#)
  (write-pair writer (vector->list vec) port))

(define (quote? obj)
  (and (pair? obj)
       (memq (car obj)
             '(quote quasiquote unquote unquote-splicing
                     syntax quasisyntax unsyntax unsyntax-splicing))))

(define (write-quote writer obj port)
  (put-string port
            (case (car obj)
              ((quote) "'")
              ((quasiquote) "`")
              ((unquote) ",")
              ((unquote-splicing) ",@")
              ((syntax) "#'")
              ((quasisyntax) "#`")
              ((unsyntax) "#,")
              ((unsyntax-splicing) "#,@")))
  (writer (cadr obj) port))

(define (write-common writer obj port)
  (cond ((quote? obj)
         (write-quote writer obj port))
        ((pair? obj)
         (write-pair writer obj port))
        ((null? obj)
         (put-string port "()"))
        ((vector? obj)
         (write-vector writer obj port))
        ((symbol? obj)
         (put-string port (symbol->string obj)))
        ((boolean? obj)
         (put-char port #\#)
         (put-char port (if obj #\t #\f)))
        ((number? obj)
         (put-string port (number->string obj)))
        (else
          (error 'write-common "unknown type" obj))))

(define char-table
  '((#\space . "#\\space")
    (#\nul . "#\\nul")
    (#\alarm . "#\\alarm")
    (#\backspace . "#\\backspace")
    (#\tab . "#\\tab")
    (#\linefeed . "#\\linefeed")
    (#\vtab . "#\\vtab")
    (#\page . "#\\page")
    (#\return . "#\\return")
    (#\esc . "#\\esc")
    (#\space . "#\\space")
    (#\delete . "#\\delete")))

(define (my-write obj port)
  (cond ((string? obj)
         (put-char port #\")
         (put-string port obj)
         (put-char port #\"))
        ((char? obj)
         (cond ((assq obj char-table)
                => (lambda (r)
                     (put-string port (cdr r))))
               (else
                (put-string port "#\\")
                (put-char port obj))))
        (else (write-common my-write obj port))))

(define (my-display obj port)
  (cond ((string? obj)
         (put-string port obj))
        ((char? obj)
         (put-char port obj))
        (else (write-common my-display obj port))))

(my-write '#\space (current-output-port))
