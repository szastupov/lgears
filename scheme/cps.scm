(library (cps)
  (export cps-convert)
  (import (rnrs)
          (only (core) pretty-print) ; This works only for ypsilon
          (quotes))

  ;For testing
  (define orig '((define (f-aux n a)
                   (if (= n 0)
                     a
                     (f-aux (- n 1) (* n a))))

                 (define (factorial n)
                   (if (= n 0)
                     1
                     (* n (factorial (- n 1)))))

                 (define (foo n)
                   (bar (lambda (x) (+ x n))))

                 (define bar (lambda (x y) (+ x y (begin (display 12) (* 1 2 3)))))

                 (define (bla)
                   (display '(a b c d)))

                 (define (pyth x y)
                   (sqrt (+ (* x x) (* y y))))))

  (define last-name 0)

  (define (gen-name)
    (let ((res (string-append "var" (number->string last-name 16))))
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

  (define (convert res node name)
    (if (pair? node)
      (case (car node)
        ((lambda)
         `(lambda (,@(cadr node) ,name)
            ,(convert-body res (cddr node) name)))

        ((if)
         (let* ((args (cdr node))
                (predname (gen-name)))
           (convert `(if ,predname
                       ,(convert '() (cadr args) name)
                       ,(convert '() (caddr args) name))
                    (car args) predname)))

        ((begin)
         (let* ((seq (reverse (cdr node)))
                (frst (car seq)))
           (fold-left (lambda (prev x)
                        (convert prev x (if (eq? x frst)
                                          name
                                          (gen-name))))
                      res seq)))

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
             `(define ,(caar def)
                (lambda (,name ,@(cdar def))
                  ,(convert-body '() (cdr def) name)))))
          ((pair? (cadr def))
           `(define ,(car def)
              ,(convert '() (cadr def) (gen-name))))
          (else def)))

  (define (convert-body res body name)
    (let* ((seq (reverse body))
           (frst (car seq)))
      (fold-left (lambda (prev x)
                   (if (eq? (car x) 'define)
                     `(,@(convert-define (cdr x)) ,prev)
                     (convert prev x (if (eq? x frst)
                                       name
                                       (gen-name)))))
                 res (reverse body))))

  (define (cps-convert source)
    (let ((res (convert-body '() source (gen-name))))
      (display "CPS: \n")
      (pretty-print res)
      (newline)
      res))

  ;(pretty-print
  ;(convert-body '() orig (gen-name))
  )
