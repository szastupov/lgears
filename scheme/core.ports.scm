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
(library (core.ports)
  (export open-port open-input-port close-port port? put-char
          put-string put-datum write get-char lookahead-char get-line
          stdin-port stdout-port stderr-port)
  (import (core.forms)
          (core.exceptions)
          (core.sequence)
          (core.arithmetic)
          (core.strings))

  (define fd-r 0)
  (define fd-r+ 1)
  (define fd-w 2)
  (define fd-w+ 3)
  (define fd-a 4)
  (define fd-a+ 5)

  (define (fd-open path mode)
    (let ((fd (builtin-call fd-open path mode)))
      (if (= fd -1)
          (error 'fd-open (describe-os-error) path mode))
      fd))

  (define (fd-close fd)
    (builtin-call fd-close fd))

  (define (fd-write fd str)
    (builtin-call fd-write fd str))

;;;
;;; FIXME: Ports must conform to r6rs
;;;

  (define (make-buffer size)
    (make-struct 'port-buffer
                 (make-string size #\space)
                 size
                 0
                 0))

  (define (buffer-string buf)
    (struct-ref buf 0))

  (define (buffer-size buf)
    (struct-ref buf 1))

  (define (buffer-rec buf)
    (struct-ref buf 2))

  (define (buffer-rec-set! buf val)
    (struct-set! buf 2 val))

  (define (buffer-pos buf)
    (struct-ref buf 3))

  (define (buffer-pos-set! buf val)
    (struct-set! buf 3 val))

  (define (make-port fd descr buffer)
    (make-struct 'port fd descr buffer))

  (define stdin-port (make-port 0 "stdin" (make-buffer 100)))
  (define stdout-port (make-port 1 "stdout" #f))
  (define stderr-port (make-port 2 "stderr" #f))

  (define (port-fd port)
    (struct-ref port 0))

  (define (port-buffer port)
    (struct-ref port 2))

  (define (open-port path mode)
    (make-port (fd-open path mode)
               path
               #f))

  (define (open-input-port path)
    (make-port (fd-open path fd-r)
               path
               (make-buffer 200)))

  (define (close-port port)
    (assert (port? port))
    (fd-close (port-fd port)))

  (define (port? obj)
    (and (struct? obj)
         (eq? (struct-type obj) 'port)
         (= (struct-size obj) 3)))

  (define (put-char port char)
    (assert (port? port))
    (assert (char? char))
    (fd-write (port-fd port)
              (string char)))

  (define (put-string port str . args)
    (assert (port? port))
    (assert (string? str))
    (let ((fd (port-fd port)))
      (case (length args)
        ((0) (fd-write fd str))
        ((1) (fd-write fd (substring str
                                     (car args)
                                     (string-length str))))
        ((2) (let ((start (car args))
                   (count (cadr args)))
               (fd-write fd (substring str start (+ start count)))))
        (else (error 'put-string "invalid-arguments" args)))))

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

  (define (quote? obj)
    (and (pair? obj)
         (memq (car obj)
               '(quote quasiquote unquote unquote-splicing
                       syntax quasisyntax unsyntax unsyntax-splicing))))

  (define (put-datum port datum)
    (assert (port? port))

    (define (put-quote obj)
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
      (dispatch (cadr obj)))

    (define (put-pair obj)
      (put-char port #\()
      (let loop ((pair obj))
        (dispatch (car pair))
        (cond ((pair? (cdr pair))
               (put-char port #\space)
               (loop (cdr pair)))
              ((null? (cdr pair)) #t)
              (else
               (put-string port " . ")
               (dispatch (cdr pair) port))))
      (put-char port #\)))

    (define (dispatch datum)
      (cond ((string? datum)
             (put-char port #\")
             (put-string port datum)
             (put-char port #\"))
            ((char? datum)
             (cond ((assq datum char-table)
                    => (lambda (r)
                         (put-string port (cdr r))))
                   (else
                    (put-string port "#\\")
                    (put-char port datum))))
            ((quote? datum)
             (put-quote datum))
            ((pair? datum)
             (put-pair datum))
            ((vector? datum)
             (put-char port #\#)
             (put-pair (vector->list datum)))
            ((null? datum)
             (put-string port "()"))
            ((symbol? datum)
             (put-string port (symbol->string datum)))
            ((boolean? datum)
             (put-string port (if datum "#t" "#f")))
            (else
             (error 'put-datum "unknown datum" datum))))
    (dispatch datum))

  (define (write obj . port)
    (put-datum
     (if (null? port)
         stdout-port
         (car port))
     obj))

  (define (port-fetch port buf)
    (let ((rec (builtin-call fd-read-string!
                             (port-fd port)
                             (buffer-string buf))))
      (buffer-rec-set! buf rec)
      (buffer-pos-set! buf 0)
      rec))

  (define (port-access-buffer p)
    (let ((buf (port-buffer p)))
      (assert buf)
      (if (= (buffer-rec buf) 0)
          (port-fetch p buf))
      buf))

  (define (get-char p)
    (assert (port? p))
    (let* ((buf (port-access-buffer p))
           (rec (buffer-rec buf)))
      (if (eof-object? rec)
          (eof-object)
          (let* ((opos (buffer-pos buf))
                 (res (string-ref (buffer-string buf)
                                  opos)))
            (buffer-pos-set! buf (+ opos 1))
            (buffer-rec-set! buf (- rec 1))
            res))))

  (define (lookahead-char p)
    (assert (port? p))
    (let ((buf (port-access-buffer p)))
      (if (eof-object? (buffer-rec buf))
          (eof-object)
          (string-ref (buffer-string buf)
                      (buffer-pos buf)))))

  (define (get-line p)
    (let loop ((res '()))
      (let ((c (get-char p)))
        (cond ((eof-object? c)
               (if (null? res)
                   (eof-object)
                   (list->string (reverse res))))
              ((eq? c #\newline)
               (list->string (reverse res)))
              (else
               (loop (cons c res)))))))
  )