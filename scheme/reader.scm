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

(library (reader)
  (export datum-from-port read-source)
  (import (rnrs)
          (format))

  (define-record-type port-proxy
    (fields port (mutable line) (mutable pos))
    (protocol
     (lambda (new)
       (lambda (nport)
         (new nport 1 0)))))

  (define (pread pp)
    (let ((ch (read-char (port-proxy-port pp))))
      (if (eqv? ch #\newline)
          (begin
            (port-proxy-line-set! pp (+ 1 (port-proxy-line pp)))
            (port-proxy-pos-set! pp 0))
          (port-proxy-pos-set! pp (+ 1 (port-proxy-pos pp))))
      ch))

  (define (ppeek pp)
    (peek-char (port-proxy-port pp)))

  (define (delimeter? x)
    (or (eof-object? x)
        (char-whitespace? x)
        (memv x '(#\( #\) #\[ #\] #\" #\# #\;))))

  (define (in-range? x y c)
    (and (char<=? x c)
         (char<=? c y)))

  (define (digit? x)
    (in-range? #\0 #\9 x))

  (define (number-start? x)
    (or (digit? x)
        (memv x '(#\+ #\-))))

  (define (hex-digit? x)
    (or (digit? x)
        (in-range? #\a #\f x)
        (in-range? #\A #\F x)))

  (define (letter? x)
    (or (in-range? #\a #\z x)
        (in-range? #\A #\Z x)))

  (define (quote? x)
    (memv x '(#\' #\` #\,)))

  (define (initial-char? x)
    (memv x '(#\! #\$ #\% #\& #\* #\/ #\: #\<
              #\= #\> #\? #\^ #\_ #\~)))

  (define (identifier-char? x)
    (or (letter? x)
        (digit? x)
        (initial-char? x)
        (memv x '(#\+ #\- #\. #\@))))

  (define (parse-escape chr)
    (case chr
      ((#\a) #\alarm)
      ((#\b) #\backspace)
      ((#\t) #\tab)
      ((#\n) #\newline)
      ((#\v) #\vtab)
      ((#\f) #\page)
      ((#\r) #\return)
      ((#\" #\\) chr)
      ;;add \x
      (else (error 'parse-escape
                   (format "Ivalid escape character ~a" chr)))))

  (define (datum-from-port port)
    (let ((pr (make-port-proxy port)))

      (define (lexical-error msg . args)
        (error 'datum-from-port
               (apply format msg args)
               (format "~a:~a"
                       (port-proxy-line pr)
                       (port-proxy-pos pr))))

      (define (read-symol start)
        (string->symbol
         (list->string
          (cons
           start
           (let loop ()
             (let ((pc (ppeek pr)))
               (cond ((delimeter? pc) '())
                     ((identifier-char? pc)
                      (cons (pread pr) (loop)))
                     (else
                      (lexical-error
                       "invalid character '~a' for identifier"
                       pc)))))))))

      (define (peculiar? pc)
        (case pc
          ((#\+ #\-)
           (delimeter? (ppeek pr)))
          ((#\.)
           (eqv? (ppeek pr) #\.))
          (else #f)))

      (define (read-peculiar pc)
        (case pc
          ((#\+) '+)
          ((#\-) '-)
          ((#\.)
           (if (and (eqv? (pread pr) #\.)
                    (eqv? (pread pr) #\.))
               '...
               (lexical-error "misplaced dot")))
          (else (lexical-error "wtf?"))))

      (define (read-string)
        (list->string
         (let loop ()
           (let ((char (pread pr)))
             (cond ((eof-object? char)
                    (lexical-error "unexpected eof while reading string"))
                   ((eqv? char #\") '())
                   ((eqv? char #\\)
                    (cons (parse-escape (pread pr))
                          (loop)))
                   (else
                    (cons char (loop))))))))

      (define (skip-comment)
        (let ((c (pread pr)))
          (unless (or (eof-object? c)
                      (eqv? c #\newline))
            (skip-comment))))


      (define (skip-block-comment)
        (let ((c (pread pr)))
          (unless (and (eqv? c #\|)
                       (eqv? (pread pr) #\#))
            (skip-block-comment))))

      (define (read-number-impl start radix)
        (let ((pred? (if (= radix 16)
                         hex-digit?
                         digit?)))
          (string->number
           (list->string
            (cons start
                  (let loop ()
                    (let ((pc (ppeek pr)))
                      (cond ((delimeter? pc) '())
                            ((pred? pc)
                             (cons (pread pr) (loop)))
                            (else
                             (lexical-error
                              "invalid characted '~a' for number" pc)))))))
           radix)))

      (define read-number
        (case-lambda
          ((start radix) (read-number-impl start radix))
          ((radix) (read-number-impl (pread pr) radix))))

      (define (syntax-type c)
        (case c
          ((#\') 'syntax)
          ((#\`) 'quasisyntax)
          ((#\,)
           (if (eq? (ppeek pr) #\@)
               (begin
                 (pread pr)
                 'usyntax-splicing)
               'unsyntax))))

      (define (read-character chr)
        (if (delimeter? (ppeek pr))
            chr
            (lexical-error "expected delimeter")))

      (define (read-sharped)
        (let ((c (pread pr)))
          (case c
            ((#\t #\T) #t)
            ((#\f #\F) #f)
            ((#\\) (read-character (pread pr)))
            ((#\b #\B) (read-number 2))
            ((#\o #\O) (read-number 8))
            ((#\d #\D) (read-number 10))
            ((#\x #\X) (read-number 16))
            ((#\' #\` #\,)
             (list (syntax-type c)
                   (dispatch (pread pr))))
            ((#\() (list->vector (read-list)))
            ((#\|)
             (skip-block-comment)
             (dispatch (pread pr)))
            (else
             (lexical-error "invalid lexical syntax #~a" c)))))

      (define (read-list)
        (let ((pc (pread pr)))
          (cond ((eof-object? pc)
                 (lexical-error "unexpected eof while reading list"))
                ((eqv? pc #\)) '())
                ((char-whitespace? pc)
                 (read-list))
                ((eqv? pc #\;)
                 (skip-comment)
                 (read-list))
                ((and (eqv? pc #\.)
                      (not (eqv? (ppeek pr) #\.)))
                 (let ((res (dispatch (pread pr))))
                   (if (eqv? (pread pr) #\))
                       res
                       (lexical-error "unexpected data after dot"))))
                (else (cons (dispatch pc)
                            (read-list))))))

      (define (read-quote pc)
        (define (quote-type)
          (case pc
            ((#\') 'quote)
            ((#\`) 'quasiquote)
            ((#\,)
             (if (eq? (ppeek pr) #\@)
                 (begin
                   (pread pr)
                   'unquote-splicing)
                 'unquote))))
        (list (quote-type)
              (dispatch (pread pr))))

      (define (dispatch pc)
        (cond ((eof-object? pc)
               (eof-object))
              ((char-whitespace? pc)
               (dispatch (pread pr)))
              ((or (letter? pc)
                   (initial-char? pc))
               (read-symol pc))
              ((peculiar? pc)
               (read-peculiar pc))
              ((number-start? pc)
               (read-number pc 10))
              ((eqv? pc #\")
               (read-string))
              ((eqv? pc #\#)
               (read-sharped))
              ((quote? pc)
               (read-quote pc))
              ((eqv? pc #\()
               (read-list))
              ((eqv? pc #\;)
               (skip-comment)
               (dispatch (pread pr)))
              ((eqv? pc #\))
               (lexical-error "unexpecter closing parenthesis"))
              (else
               (lexical-error "invalid lexical ~a" pc))))

      (dispatch (pread pr))))

  (define (read-str buf)
    (call-with-port
     (open-string-input-port buf)
     datum-from-port))

  (define (read-source path)
    (call-with-input-file path
      (lambda (port)
        (let loop ((res '())
                   (datum (datum-from-port port)))
          (if (eof-object? datum)
              (reverse res)
              (loop (cons datum res) (datum-from-port port)))))))
  )
