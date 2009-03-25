(library (sc expand)
  (export expand)
  (import (rnrs)
          (rnrs eval)
          (core))

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
          (eval (caddr x) (environment '(rnrs)))))

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
          (if (pair? x)
            (cond ((expand? (car x))
                   => (lambda (f)
                        (let ((res (f x)))
                          ;(format #t "Expanded ~s\n" res)
                          (scan-node res))))
                  (else
                    (case (car x)
                      ((lambda)
                       `(lambda ,(cadr x) ,@(expand env (cddr x))))
                      ((define)
                       (expand-define (cdr x)))
                      ((define-syntax)
                       (error 'scan-node "Bug!" x))
                      (else
                        (map scan-node x)))))
            x))

        (map scan-node source))))

  ;(pretty-print (expand '() (read-source "test.scm")))
  )
