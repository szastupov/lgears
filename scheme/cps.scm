(library (cps)
  (export cps-convert)
  (import (rnrs)
          (only (core) pretty-print) ; This works only for ypsilon
          (quotes))

  ;For testing
  (define orig '(
                 (define (f-aux n a)
                   (if (= n 0)
                     a
                     (f-aux (- n 1) (* n a))))

                 (define (factorial n)
                   (if (= n 0)
                     1
                     (* n (factorial (- n 1)))))

                 (define (foo n)
                   (bar (lambda (x) (+ x n))))

                 (define (bla)
                   (display '(a b c d)))

                 (define (pyth x y)
                   (sqrt (+ (* x x) (* y y))))

                 (define abrabar)

                 (bla 10)
                 (foo 'bar)
                 700
                 ))


  (define last-name 0)

  (define (gen-name)
    (let ((res (string-append "_var" (number->string last-name 16))))
      (set! last-name (+ last-name 1))
      (string->symbol res)))

  (define (echo n)
    (display n)
    (newline))

  (define (self-eval? node)
    (or (not (pair? node))
        (and (eq? (car node) 'quote)
             (not (pair? (cadr node))))))

  (define (convert-quote res node func name)
    (if (pair? node)
      (convert res (func node) name)
      node))

  (define (convert-func res args body name)
    `(lambda (,name ,@args)
       ,(convert-body res body name)))

  (define (convert res node name)
    (if (pair? node)
      (case (car node)
        ((lambda)
         (convert-func res (cadr node) (cddr node) name))

        ((if)
         (let* ((args (cdr node))
                (pred (car args))
                (predname (if (self-eval? pred)
                            pred (gen-name)))
                (expr `(if ,predname
                       ,(convert '() (cadr args) name)
                       ,@(if (null? (cddr args))
                          '()
                          (list (convert '() (caddr args) name))))))
           (if (self-eval? pred)
             expr
             (convert expr pred predname))))

        ((begin)
         (convert-seq res (cdr node) name))

        ((define)
         (error 'convert "misplaced defination"))

        ((quote)
         (convert-quote res (cadr node) trquote name))
        ((quasiquote)
         (convert-quote res (cadr node) trquasiquote name))

        (else
          (let* ((args (reverse (cdr node)))
                 (largs (map (lambda (x)
                               (if (self-eval? x)
                                 x
                                 (gen-name)))
                             args))
                 (control (if (null? res)
                            name
                            `(lambda (,name) ,res)))
                 (expr `(,(car node) ,control ,@(reverse largs))))
            (fold-left (lambda (prev x n)
                         (if (self-eval? x)
                           prev
                           (convert prev x n)))
                       expr args largs))))
      (if (null? res)
        (list name node)
        `((lambda (,name) ,res) ,node))))

  (define (convert-define def)
    (cond ((pair? (car def))
           (let ((name (gen-name)))
             `(set! ,(caar def)
                ,(convert-func '() (cdar def) (cdr def) name))))
          ((pair? (cadr def))
           `(set! ,(car def)
              ,(convert '() (cadr def) (gen-name))))
          (else def)))

  (define (convert-seq res source name)
    (if (null? source)
      '()
      (let* ((seq (reverse source))
             (frst (car seq)))
        (fold-left (lambda (prev x)
                     (convert prev x (if (eq? x frst)
                                       name
                                       (gen-name))))
                   res seq))))

  (define (defination? x)
    (and (pair? x) (eq? (car x) 'define)))

  (define (convert-body res body name)
    (let-values (((defines expressions)
                  (partition defination? body)))
      (let ((init `((extend ,(map (lambda (x)
                                    (if (pair? (cadr x))
                                      (caadr x)
                                      (cadr x)))
                                  defines))
                    ,@(fold-left (lambda (prev x)
                                   (if (null? (cddr x))
                                     prev
                                     (cons (convert-define (cdr x)) prev)))
                                 '() (reverse defines))))
            (rest (convert-seq '() expressions name)))
        (if (null? defines)
          rest
          (append init
                  (if (null? rest)
                    '() (list rest)))))))

  (define (cps-convert source)
    (let ((res (convert-body '() source '__exit)))
      (display "CPS: \n")
      (pretty-print res)
      (newline)
      (if (pair? (car res))
        res
        (list res))))

  ;(pretty-print (convert-body '() orig (gen-name)))
  )
