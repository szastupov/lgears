(library (cps)
  (export cps-convert)
  (import (rnrs)
          (only (core) pretty-print) ; This works only for ypsilon
          (trace)
          (quotes))

  ;For testing
  (define orig '(
                 #|
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

                 (define const 12)
                 (define lst (cons 'a (cons 'b 'c)))

                 (bla 10)
                 (letrec ((foo (lambda (x) (bar x)))
                          (bar (lambda (y) (foo y))))
                   (foo bar)
                   (display "ok"))
                 |#
                 (display '(1 2 3 . 4))
                 ))


  (define last-name 0)

  (define (gen-name)
    (let ((res (string-append "_var" (number->string last-name 16))))
      (set! last-name (+ last-name 1))
      (string->symbol res)))

  (define (self-eval? node)
    (or (not (pair? node))
        (and (eq? (car node) 'quote)
             (not (pair? (cadr node))))))

  (define (convert-quote res node func name)
    (if (pair? node)
      (convert res (func node) name)
      node))

  (define (convert-func args body)
    (let ((name (gen-name)))
      `(lambda (,name ,@args)
         ,@(convert-body body name))))

  (define (unletrec node)
    `(,@(map (lambda (x)
               `(define ,(car x) ,(cadr x)))
             (cadr node))
       ,@(cddr node)))

  (define (convert res node name)
    (if (pair? node)
      (case (car node)
        ((lambda) ;FIXME inline lambda if possible
         (let ((func (convert-func (cadr node) (cddr node))))
           (if (null? res)
             (list name func)
             `((lambda (,name) ,res) ,func))))

        ((letrec)
         (let ((func (convert-func '() (unletrec node))))
           (if (null? res)
             (list name func)
             `(,func (lambda (,name) ,res)))))

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
                 (control (cond ((null? res)
                                 name)
                                ((and (pair? (car res))
                                      (eq? (caar res) 'set!))
                                 `(lambda (,name) ,@res))
                                (else
                                  `(lambda (,name) ,res))))
                 (expr `(,(car node) ,control ,@(reverse largs))))
            (fold-left (lambda (prev x n)
                         (if (self-eval? x)
                           prev
                           (convert prev x n)))
                       expr args largs))))
      (if (null? res)
        (list name node)
        `((lambda (,name) ,res) ,node))))

  (define (convert-define res def)
    (cond ((pair? (car def))
           `((set! ,(caar def)
               ,(convert-func (cdar def) (cdr def)))
             ,@res))
          ((self-eval? (cadr def))
           `((set! ,(car def) ,(cadr def))
             ,@res))
          ((pair? (cadr def))
           (let* ((name (gen-name))
                  (expr `((set! ,(car def) ,name)
                          ,@res)))
             (list (convert expr (cadr def) name))))))

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

  (define (convert-body body name)
    (let-values (((defines expressions)
                  (partition defination? body)))
      (if (null? expressions)
        (error 'convert-body "empty body"))
      (let* ((rest (convert-seq '() expressions name))
             (extend (if (null? defines)
                       '()
                       `((extend ,(map (lambda (x)
                                         (if (pair? (cadr x))
                                           (caadr x)
                                           (cadr x)))
                                       defines))))))
        `(,@extend
           ,@(fold-left (lambda (prev x)
                          (if (null? (cddr x))
                            prev
                            (convert-define prev (cdr x))))
                        (list rest) (reverse defines))))))

  (define (cps-convert source)
    (let ((res (convert-body source '__exit)))
      (display "CPS: \n")
      (pretty-print res)
      (newline)
      (if (pair? (car res))
        res
        (list res))))

  (define (read-source file)
    (let* ((port (open-file-input-port file (file-options no-fail) (buffer-mode block) (native-transcoder)))
           (res (get-datum port)))
      (close-port port)
      res))

  ;(define p (read-source "/home/redchrom/psyntax.pp"))
  ;(pretty-print (convert-body p (gen-name)))

  ;(pretty-print (convert-body orig (gen-name)))
  )
