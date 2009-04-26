(library (reader)
  (export datum-from-port read-source)
  (import (rnrs base)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs unicode)
          (rnrs lists)
          (rnrs control)
          (format))

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

    (define (lexical-error msg . args)
      (error 'datum-from-port
             (apply format msg args)
             (port-position port)))

    (define (read-symol start)
      (string->symbol
       (list->string
        (cons
         start
         (let loop ()
           (let ((pc (peek-char port)))
             (cond ((delimeter? pc) '())
                   ((identifier-char? pc)
                    (cons (read-char port) (loop)))
                   (else
                    (lexical-error
                     "invalid character '~a' for identifier"
                     pc)))))))))

    (define (peculiar? pc)
      (case pc
        ((#\+ #\-)
         (delimeter? (peek-char port)))
        ((#\.)
         (eqv? (peek-char port) #\.))
        (else #f)))

    (define (read-peculiar pc)
      (case pc
        ((#\+) '+)
        ((#\-) '-)
        ((#\.)
         (if (and (eqv? (read-char port) #\.)
                  (eqv? (read-char port) #\.))
             '...
             (lexical-error "misplaced dot")))
        (else (lexical-error "wtf?"))))

    (define (read-string)
      (list->string
       (let loop ()
         (let ((char (read-char port)))
           (cond ((eof-object? char)
                  (lexical-error "unexpected eof while reading string"))
                 ((eqv? char #\") '())
                 ((eqv? char #\\)
                  (cons (parse-escape (read-char port))
                        (loop)))
                 (else
                  (cons char (loop))))))))

    (define (skip-comment)
      (let ((c (read-char port)))
        (unless (or (eof-object? c)
                    (eqv? c #\newline))
          (skip-comment))))

    (define (read-number-impl start radix)
      (let ((pred? (if (= radix 16)
                       hex-digit?
                       digit?)))
        (string->number
         (list->string
          (cons start
                (let loop ()
                  (let ((pc (peek-char port)))
                    (cond ((delimeter? pc) '())
                          ((pred? pc)
                           (cons (read-char port) (loop)))
                          (else
                           (lexical-error
                            "invalid characted '~a' for number" pc)))))))
         radix)))

    (define read-number
      (case-lambda
        ((start radix) (read-number-impl start radix))
        ((radix) (read-number-impl (read-char port) radix))))

    (define (syntax-type c)
      (case c
        ((#\') 'syntax)
        ((#\`) 'quasisyntax)
        ((#\,)
         (if (eq? (peek-char port) #\@)
             (begin
               (read-char port)
               'usyntax-splicing)
             'unsyntax))))

    (define (read-character chr)
      (if (delimeter? (peek-char port))
          chr
          (lexical-error "expected delimeter")))

    (define (read-sharped)
      (let ((c (read-char port)))
        (case c
          ((#\t #\T) #t)
          ((#\f #\F) #f)
          ((#\\) (read-character (read-char port)))
          ((#\b #\B) (read-number 2))
          ((#\o #\O) (read-number 8))
          ((#\d #\D) (read-number 10))
          ((#\x #\X) (read-number 16))
          ((#\' #\` #\,)
           (list (syntax-type c)
                 (dispatch (read-char port))))
          ((#\() (list->vector (read-list)))
          (else
           (lexical-error "invalid lexical syntax #~a" c)))))

    (define (read-list)
      (let ((pc (read-char port)))
        (cond ((eof-object? pc)
               (lexical-error "unexpected eof while reading list"))
              ((eqv? pc #\)) '())
              ((char-whitespace? pc)
               (read-list))
              ((eqv? pc #\;)
               (skip-comment)
               (read-list))
              ((and (eqv? pc #\.)
                    (not (eqv? (peek-char port) #\.)))
               (let ((res (dispatch (read-char port))))
                 (if (eqv? (read-char port) #\))
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
           (if (eq? (peek-char port) #\@)
               (begin
                 (read-char port)
                 'unquote-splicing)
               'unquote))))
      (list (quote-type)
            (dispatch (read-char port))))

    (define (dispatch pc)
      (cond ((eof-object? pc)
             (eof-object))
            ((char-whitespace? pc)
             (dispatch (read-char port)))
            ((number-start? pc)
             (read-number pc 10))
            ((or (letter? pc)
                 (initial-char? pc))
             (read-symol pc))
            ((peculiar? pc)
             (read-peculiar pc))
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
             (dispatch (read-char port)))
            ((eqv? pc #\))
             (lexical-error "unexpecter closing parenthesis"))
            (else
             (lexical-error "invalid lexical ~a" pc))))

    (dispatch (read-char port)))

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
