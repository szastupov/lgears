(library (sc expand)
  (export expand)
  (import (rnrs)
          (rnrs eval)
          (format)
          (only (core) pretty-print))

  (define-record-type eenv
    (fields parent tbl)
    (protocol
      (lambda (new)
        (lambda (prev expanders)
          (let ((ntbl (make-eq-hashtable)))
            (for-each (lambda (x)
                        (hashtable-set! ntbl (car x) (cdr x)))
                      expanders)
            (new prev ntbl))))))

  (define (eenv-install env name expander)
    (hashtable-set! (eenv-tbl env) name expander))

  (define (define-syntax? x)
    (and (pair? x)
         (eq? (car x) 'define-syntax)))

  (define (macro->expander m)
    (lambda (x e) (e (m x) e)))

  (define (compile-macro x)
    (cons (cadr x)
          (macro->expander
           (eval (caddr x) (environment '(rnrs)
                                        '(sc gen-name)
                                        '(format))))))
   (define (splice-begin src)
    (let loop ((cur src))
      (cond ((null? cur)
             '())
            ((and (pair? cur) (pair? (car cur))
                  (eq? (caar cur) 'begin))
             (append (cdar cur) (loop (cdr cur))))
            (else
              (cons (car cur) (loop (cdr cur)))))))

  (define (expand prev node)
    (let-values (((expanders source) (partition define-syntax?
                                                (splice-begin node))))
      (let ((env (make-eenv prev (map compile-macro expanders))))

        (define (expand? x)
          (let loop ((cur-env env))
            (cond ((null? cur-env)
                   #f)
                  ((hashtable-contains? (eenv-tbl cur-env) x)
                   (hashtable-ref (eenv-tbl cur-env) x #f))
                  (else
                    (loop (eenv-parent cur-env))))))

        (define (initial-expander x e)
          (cond ((not (pair? x)) x)
                ((expand? (car x)) => (lambda (f)
                                        (f x e)))
                (else (map (lambda (x) (e x e)) x))))

        (eenv-install env 'quote (lambda (x e) x))
        (eenv-install env 'lambda (lambda (x e)
                                    `(lambda ,(cadr x)
                                       ,@(expand env (cddr x)))))
        (eenv-install env 'define (lambda (x e)
                                    (if (pair? (cadr x))
                                        `(define ,(caadr x)
                                           ,(e `(lambda ,(cdadr x)
                                                  ,@(cddr x)) e))
                                        `(define ,(cadr x)
                                           ,(e (caddr x) e)))))
        (map (lambda (x)
               (initial-expander x initial-expander)) source))))

  ;(pretty-print (expand '() (read-source "test.scm")))
  )
