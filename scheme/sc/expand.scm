(library (sc expand)
  (export expand)
  (import (rnrs)
          (rnrs eval)
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

  (define (define-syntax? x)
    (and (pair? x)
         (eq? (car x) 'define-syntax)))

  (define (compile-expander x)
    (cons (cadr x)
          (eval (caddr x) (environment '(rnrs)
                                       '(format)
                                       '(sc quotes)))))

  (define (expand prev node)
    (let-values (((expanders source) (partition define-syntax? node)))
      (let ((env (make-eenv prev (map compile-expander expanders))))

        (define (expand? x)
          (let loop ((cur-env env))
            (cond ((null? cur-env)
                   #f)
                  ((hashtable-contains? (eenv-tbl cur-env) x)
                   (hashtable-ref (eenv-tbl cur-env) x #f))
                  (else
                    (loop (eenv-parent cur-env))))))

        (define (expand-define x)
          (if (pair? (car x))
            `(define ,(caar x)
               (lambda ,(cdar x) ,@(map scan-node (expand env (cdr x)))))
            `(define ,@(expand env x))))

        (define (scan-node x)
          (cond ((not (pair? x))
                 x)
                ((expand? (car x)) => (lambda (f)
                                        (let ((res (f x)))
                                          (scan-node res))))
                (else (case (car x)
                        ((quote) x)
                        ((lambda)
                         `(lambda ,(cadr x) ,@(expand env (cddr x))))
                        ((if)
                         `(if ,@(expand env (cdr x))))
                        ((set!)
                         `(set! ,(cadr x) ,(expand env (caddr x))))
                        ((define)
                         (expand-define (cdr x)))
                        (else
                          (map scan-node x))))))

        (map scan-node source))))

  ;(pretty-print (expand '() (read-source "test.scm")))
  )
