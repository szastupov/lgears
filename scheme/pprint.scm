(library (pprint)
  (export pprint)
  (import (rnrs))

  (define max-width 100)

  (define (indent-type1? x)
    (memq x '(define library define-record-type define-syntax
               syntax-rules syntax-case with-syntax lambda let-syntax
               letrec-syntax let letrec let* letrec* let-values let*-values)))

  (define (indent-type2? x)
    (memq x '(if cond case and or set! import export cons map for-each
                 exists for-all)))

  (define-record-type sexpr
    (fields width datum flags)
    (protocol
     (lambda (new)
       (case-lambda
         ((w d) (new w d #f))
         ((w d f) (new w d f))))))

  (define (datum-width dt)
    (cond ((string? dt)
           (string-length dt))
          ((symbol? dt)
           (string-length (symbol->string dt)))
          ((number? dt)
           (string-length (number->string dt)))
          ((char? dt) 1)
          ((null? dt) 2)
          ((boolean? dt) 2)
          (else
           (error 'datum-width "unknown" dt))))

  (define (parse-pairs dt quoted?)
    (cond ((null? dt) '())
          ((pair? dt)
           (cons (parse (car dt) quoted?)
                 (parse-pairs (cdr dt)
                              (if (eq? (car dt) 'quote)
                                  #t quoted?))))
          (else
           (parse dt quoted?))))

  (define (sum-pairs dt)
    (let loop ((sum 0)
               (dt dt))
      (cond ((null? dt) sum)
            ((pair? dt)
             (loop (+ sum (sexpr-width (car dt)))
                   (cdr dt)))
            (else
             (+ sum (sexpr-width dt))))))

  (define (check-flags dt quoted?)
    (cond (quoted? #f)
          ((symbol? dt)
           (cond ((indent-type1? dt) 'nest)
                 ((indent-type2? dt) 'group)
                 (else #f)))
          (else #f)))

  (define (parse dt quoted?)
    (cond ((pair? dt)
           (let ((parsed (parse-pairs dt quoted?)))
             (make-sexpr (sum-pairs parsed)
                         parsed)))
          ((vector? dt)
           (let ((parsed (parse-pairs (vector->list dt) #t)))
             (make-sexpr (sum-pairs parsed)
                         (list->vector parsed))))
          (else
           (make-sexpr (datum-width dt)
                       dt
                       (check-flags dt quoted?)))))

  (define (strip x)
    (cond ((pair? x)
           (cons (strip (car x))
                 (strip (cdr x))))
          ((vector? x)
           (list->vector (strip (vector->list x))))
          ((sexpr? x)
           (strip (sexpr-datum x)))
          (else x)))

  (define (nest? x)
    (eq? (sexpr-flags x) 'nest))

  (define (group? x)
    (eq? (sexpr-flags x) 'group))

  (define (sexpr-pair? x)
    (pair? (sexpr-datum x)))

  (define (sexpr-vector? x)
    (vector? (sexpr-datum x)))

  (define (fits? shift x)
    (>= max-width
        (+ (sexpr-width x) shift)))

  (define (quote? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (memq (sexpr-datum (car x))
               '(quote quasiquote unquote unquote-splicing syntax
                       quasisyntax unsyntax unsyntax-splicing))))


  (define (pprint-impl x port)

    (define (indent width)
      (unless (<= width 0)
        (display " " port)
        (indent (- width 1))))

    (define (print-quoted shift dt)
      (define (quote-str)
        (case (sexpr-datum (car dt))
          ((quote) "'")
          ((quasiquote) "`")
          ((unquote) ",")
          ((unquote-splicing) ",@")
          ((syntax) "#'")
          ((quasisyntax) "#`")
          ((unsyntax) ",")
          ((unsyntax-splicing) ",@")))
      (let* ((str (quote-str))
             (nshift (+ shift (string-length str))))
        (put-string port str)
        (print #f nshift (cadr dt))))

    (define (print indent? shift x)
      (if indent?
          (indent shift))
      (if (fits? shift x)
          (write (strip x) port)
          (cond ((sexpr-pair? x)
                 (let ((datum (sexpr-datum x)))
                   (if (quote? datum)
                       (print-quoted shift datum)
                       (begin
                         (put-string port "(")
                         (print-pairs (+ 1 shift) datum)))))
                ((sexpr-vector? x)
                 (put-string port "#(")
                 (print-pairs (+ 2 shift) (vector->list (sexpr-datum x))))
                (else
                 (write (strip x))))))

    (define (print-flagged shift nshift x)
      (if (fits? shift (car x))
          (print #t 1 (car x))
          (begin
            (put-string port " (")
            (print-pairs-impl #t (+ 1 nshift) (sexpr-datum (car x)))))
      (newline port)
      (print-pairs-impl #f nshift (cdr x)))

    (define (print-pairs-impl first? shift x)
      (print (not first?) shift (car x))
      (cond ((null? (cdr x))
             (put-string port ")"))
            ((pair? (cdr x))
             (cond ((group? (car x))
                    (print-flagged shift
                                   (+ (sexpr-width (car x)) shift 1)
                                   (cdr x)))
                   ((nest? (car x))
                    (print-flagged shift
                                   (+ 1 shift)
                                   (cdr x)))
                   (else
                    (newline port)
                    (print-pairs-impl #f shift (cdr x)))))
            (else
             (newline port)
             (indent shift)
             (put-string port ".\n")
             (print #t shift (cdr x))
             (put-string port ")"))))

    (define (print-pairs shift x)
      (print-pairs-impl #t shift x))

    (print #t 0 (parse x #f)))

  (define pprint
    (case-lambda
      ((dt) (pprint-impl dt (current-output-port)))
      ((dt port) (pprint-impl dt port))))

  )