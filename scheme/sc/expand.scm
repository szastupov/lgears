(library (sc expand)
  (export expand)
  (import (rnrs)
          (rnrs eval)
          (format)
          (sc gen-name)
          (only (core) pretty-print))

  (define-record-type binding
    (fields type value))

  (define (macro x)
    (make-binding 'macro x))

  (define (lexical x)
    (make-binding 'lexical x))

  (define-record-type bind-env
    (fields parent tbl)
    (protocol
      (lambda (new)
        (lambda (prev bindings)
          (let ((ntbl (make-eq-hashtable)))
            (for-each (lambda (x)
                        (hashtable-set! ntbl (car x) (cdr x)))
                      bindings)
            (new prev ntbl))))))

  (define (bind-env-install env name binding)
    (hashtable-set! (bind-env-tbl env) name binding))

  (define (install-expander env name expander)
    (bind-env-install env name (make-binding 'macro expander)))

  (define (install-lexixal env name)
    (let ((nname (gen-name name)))
      (bind-env-install env name (make-binding 'lexical nname))
      nname))

  (define (define-syntax? x)
    (and (pair? x)
         (eq? (car x) 'define-syntax)))

  (define (make-expander m)
    (make-binding 'macro
                  (lambda (x e) (e (m x) e))))

  (define (compile-macro x)
    (cons (cadr x)
          (make-expander
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

  (define (install-top-level env)
    (install-expander env 'quote (lambda (x e) x))
    (install-expander env 'lambda (lambda (x e)
                                    (expand env (cadr x) (cddr x))))
    (install-expander env 'define (lambda (x e)
                                    (if (pair? (cadr x))
                                      `(define ,(caadr x)
                                         ,(e `(lambda ,(cdadr x)
                                                ,@(cddr x)) e))
                                      `(define ,(cadr x)
                                         ,(e (caddr x) e))))))

  (define (expand prev vars body)
    (let-values (((expanders source) (partition define-syntax?
                                                (splice-begin body))))
      (let ((env (make-bind-env prev (map compile-macro expanders))))

        (define (binding? x)
          (let loop ((cur-env env))
            (cond ((null? cur-env)
                   #f)
                  ((hashtable-contains? (bind-env-tbl cur-env) x)
                   (hashtable-ref (bind-env-tbl cur-env) x #f))
                  (else
                    (loop (bind-env-parent cur-env))))))

        (define (bind-type x type)
          (cond ((binding? x) => (lambda (b)
                                   (if (eq? (binding-type b) type)
                                     (binding-value b) #f)))
                (else #f)))

        (define (initial-expander x e)
          (cond ((symbol? x)
                 (cond ((bind-type x 'lexical) => (lambda (x) x))
                       (else x)))
                ((not (pair? x)) x)
                ((bind-type (car x) 'macro) => (lambda (b)
                                                 (b x e)))
                (else (map (lambda (x) (e x e)) x))))

        (install-top-level env)
        (let ((newbody (map (lambda (x)
                              (initial-expander x initial-expander))
                            source)))
          (if (null? prev)
            newbody
            `(lambda ,(map (lambda (var)
                             (install-lexixal env var))
                           vars)
               ,@newbody))))))

  ;(pretty-print (expand '() (read-source "test.scm")))
  )
