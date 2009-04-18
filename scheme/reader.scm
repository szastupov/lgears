(define (delimeter? x)
  (or (eof-object? x)
      (char-whitespace? x)
      (memq x '(#\( #\) #\[ #\] #\" #\# #\;))))

(define (in-range? x y c)
  (and (char<=? x c)
       (char<=? c y)))

(define (digit? x)
  (in-range? #\0 #\9 x))

(define (hex-digit? x)
  (or (digit? x)
      (in-range? #\a #\f x)
      (in-range? #\A #\F x)))

(define (letter? x)
  (or (in-range? #\a #\z x)
      (in-range? #\A #\Z x)))

(define (quote? x)
  (memq x '(#\' #\` #\,)))

(define (identifier-sym? x)
  (or (letter? x)
      (digit? x)
      (memq x '(#\! #\$ #\% #\& #\* #\/ #\: #\<
                #\= #\> #\? #\^ #\_ #\~ #\+ #\-))))

(define (rlist->string lst)
  (list->string (reverse lst)))

(define (datum-from-port port)  
  (define (read-symol)
    (string->symbol
     (list->string
      (let loop ()
        (let ((pc (peek-char port)))
          (cond ((delimeter? pc) '())
                ((identifier-sym? pc)
                 (cons (read-char port)
                       (loop)))
                (else (error pc "unexpected symbol"))))))))

  (define (read-string)
    (read-char port)
    (list->string
     (let loop ()
       (let ((char (get-char port)))
         (cond ((eof-object? char)
                (error 'read-string "unexpected eof"))
               ((char=? char #\") '())
               (else
                (cons char (loop))))))))

  (define (skip-comment)
    (let ((c (read-char port)))
      (unless (or (eof-object? c)
                  (char=? c #\newline))
        (skip-comment))))

  (define (read-number radix)
    (let ((pred? (if (= radix 16)
                     hex-digit?
                     digit?)))
      (string->number
       (list->string
        (let loop ()
          (let ((pc (peek-char port)))
            (cond ((delimeter? pc) '())
                  ((pred? pc)
                   (cons (read-char port)
                         (loop)))
                  (else (error pc "unexpected symbol"))))))
       radix)))

  (define (syntax-type c)
    (format #t "~a\n" (peek-char port))
    (case c
      ((#\') 'syntax)
      ((#\`) 'quasisyntax)
      ((#\,)
       (if (eq? (peek-char port) #\@)
           (begin
             (read-char port)
             'usyntax-splicing)
           'unsyntax))))

  (define (read-sharped)
    (read-char port)
    (let ((c (read-char port)))
      (case c
        ((#\t #\T) #t)
        ((#\f #\F) #f)
        ((#\\) (read-char port))
        ((#\b #\B) (read-number 2))
        ((#\o #\O) (read-number 8))
        ((#\x #\X) (read-number 16))
        ((#\' #\` #\,)
         (list (syntax-type c)
               (dispatch (peek-char port))))
        ((#\() (list->vector (read-list)))
        (else (error c "unexpected symbol")))))

  (define (read-list)
    (let ((pc (peek-char port)))
      (cond ((char=? pc #\))
             (read-char port)
             '())
            ((char-whitespace? pc)
             (read-char port)
             (read-list))
            ((char=? pc #\.)
             (read-char port)
             (let ((res (dispatch (peek-char port))))
               (if (char=? (read-char port) #\))
                   res
                   (error 'read-list "unexpected data after dot"))))
            ((eof-object? pc)
             (error 'read-list "unexpected eof"))
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
    (read-char port)
    (list (quote-type)
          (dispatch (peek-char port))))

  (define (dispatch pc)
    (cond ((eof-object? pc) (eof-object))
          ((char-whitespace? pc)
           (read-char port)
           (dispatch (peek-char port)))
          ((digit? pc) (read-number 10))
          ((letter? pc) (read-symol))
          ((char=? pc #\") (read-string))
          ((char=? pc #\#) (read-sharped))
          ((quote? pc) (read-quote pc))
          ((char=? pc #\()
           (read-char port)
           (read-list))
          ((char=? pc #\;)
           (skip-comment)
           (dispatch (peek-char port)))
          (else (error pc "unexpected symbol"))))
  
  (dispatch (peek-char port)))

(call-with-port
 (open-string-input-port ";comment\n(\"111\" #xFF  ,(foo bar #t 12) . ff)")
 datum-from-port)