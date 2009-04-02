(library (sc expand)
  (export expand)
  (import (except (rnrs) identifier? ...)
          (rnrs eval)
          (format)
          (sc gen-name)
          (only (core) pretty-print))

  (define-record-type syntax-object
    (fields expr wrap))

  (define-record-type mark)

  (define-record-type subst
    (fields sym mark* label))

  (define-record-type label)

  (define make-binding cons)
  (define binding-type car)
  (define binding-value cdr)

  (define top-mark (make-mark))

  (define (top-marked? wrap)
    (and (not (null? wrap))
         (or (eq? (car wrap) top-mark)
             (top-marked? (cdr wrap)))))

  (define (strip x)
    (cond ((syntax-object? x)
           (if (top-marked? (syntax-object-wrap x))
               (syntax-object-expr x)
               (strip (syntax-object-expr x))))
          ((pair? x)
           (let ((a (strip (car x)))
                 (d (strip (cdr x))))
             (if (and (eq? a (car x)) (eq? d (cdr x)))
                 x
                 (cons a d))))
          (else x)))

  (define (syntax-error what msg)
    (error (strip what) msg))

  (define (identifier? x)
    (and (syntax-object? x)
         (symbol? (syntax-object-expr x))))

  (define (ellipsis? x)
    (and (identifier? x)
         (eq? (syntax-object-expr x) '...)))

  (define (syntax-pair? x)
    (pair? (syntax-object-expr x)))

  (define (syntax-car x)
    (extend-wrap
     (syntax-object-wrap x)
     (car (syntax-object-expr x))))

  (define (syntax-cdr x)
    (extend-wrap
     (syntax-object-wrap x)
     (cdr (syntax-object-expr x))))

  (define (syntax-cadr x)
    (syntax-car (syntax-cdr x)))

  (define (syntax-cddr x)
    (syntax-cdr (syntax-cdr x)))

  (define (syntax-caddr x)
    (syntax-car (syntax-cddr x)))

  (define (syntax-null? x)
    (null? (syntax-object-expr x)))

  (define (syntax->list x)
    (if (syntax-null? x)
        '()
        (cons (syntax-car x)
              (syntax->list (syntax-cdr x)))))

  (define (syntax-length x)
    (length (syntax->list x)))

  (define (self-evaluating? x)
    (not (or (pair? x)
             (syntax-object? x))))

  (define (wrap-marks wrap)
    (if (null? wrap)
      '()
      (let ((w0 (car wrap)))
        (if (mark? w0)
          (cons w0 (wrap-marks (cdr wrap)))
          (wrap-marks (cdr wrap))))))

  (define (same-marks? m1* m2*)
    (if (null? m1*)
        (null? m2*)
        (and (not (null? m2*))
             (eq? (car m1*) (car m2*))
             (same-marks? (cdr m1*) (cdr m2*)))))

  (define (extend-wrap wrap x)
    (if (syntax-object? x)
        (make-syntax-object
         (syntax-object-expr x)
         (join-wraps wrap (syntax-object-wrap x)))
        (make-syntax-object x wrap)))

  (define (join-wraps wrap1 wrap2)
    (cond ((null? wrap1) wrap2)
          ((null? wrap2) wrap1)
          (else
           (let loop ((w (car wrap1))
                      (w* (cdr wrap1)))
             (if (null? w*)
                 (if (and (mark? w)
                          (eq? (car wrap2) w))
                     (cdr wrap2)
                     (cons w wrap2))
                 (cons w (loop (car w*) (cdr w*))))))))

  (define (add-mark mark x)
    (extend-wrap (list mark) x))

  (define (add-subst id label x)
    (extend-wrap
     (list (make-subst
            (syntax-object-expr id)
            (wrap-marks (syntax-object-wrap id))
            label))
     x))

  (define (extend-env label binding env)
    (cons (cons label binding) env))

  (define (id-binding id r)
    (label-binding id (id-label id) r))

  (define (id-label id)
    (let ((sym (syntax-object-expr id))
          (wrap (syntax-object-wrap id)))
      (let search ((wrap wrap)
                   (mark* (wrap-marks wrap)))
        (if (null? wrap)
            (syntax-error id "undefined identifier")
            (let ((w0 (car wrap)))
              (if (mark? w0)
                  (search (cdr wrap) (cdr mark*))
                  (if (and (eq? (subst-sym w0) sym)
                           (same-marks? (subst-mark* w0) mark*))
                      (subst-label w0)
                      (search (cdr wrap) mark*))))))))

  (define (label-binding id label r)
    (let ((a (assq label r)))
      (if a (cdr a)
          (syntax-error id "displaced lexical"))))

  (define (exp-macro p x)
    (let* ((m (make-mark))
           (xm (add-mark m x)))
      (add-mark m (extend-wrap
                   (syntax-object-wrap xm)
                   (p xm)))))

  (define (exp-core p x r mr)
    (p x r mr))

  (define (exp-exprs x* r mr)
    (map (lambda (x)
           (exp-dispatch x r mr))
         x*))

  (define (exp-dispatch x r mr)
    (cond ((identifier? x)
           (let ((b (id-binding x r)))
             (case (binding-type b)
               ((macro) (exp-dispatch (exp-macro (binding-value b) x) r mr))
               ((lexical)(binding-value b))
               (else (syntax-error x "invalid syntax")))))
          ((not (syntax-pair? x))
           (strip x))
          ((identifier? (syntax-car x))
           (let ((b (id-binding (syntax-car x) r)))
             (case (binding-type b)
               ((macro) (exp-dispatch (exp-macro (binding-value b) x) r mr))
               ((lexical)
                `(,(binding-value b)
                  ,@(exp-exprs (syntax->list (syntax-cdr x)) r mr)))
               ((core) (exp-core (binding-value b) x r mr))
               (else (syntax-error x "invalid syntax")))))
          (else
           `(,(exp-dispatch (syntax-car x) r mr)
             ,@(exp-exprs (syntax->list (syntax-cdr x)) r mr)))))

  (define (get-vars res x pat)
    (if (pair? pat)
        (fold-left get-vars res (syntax->list x) pat)
        (cons x res)))

  (define (syntax-dispatch x reserved . rules)
    (define (match? xpr pat)
      (cond ((pair? pat)
             (if (and (syntax-pair? xpr)
                      (= (length pat) (syntax-length xpr)))
                 (for-all match? (syntax->list xpr) pat)
                 #f))
            (else #t)))
    (let loop ((rules rules))
      (cond ((null? rules)
             (syntax-error x "ivalid syntax"))
            ((match? x (caar rules))
             (format #t "matched ~a!\n" (caar rules))
                          (apply (cdar rules)
                                  (cdr (reverse (get-vars '() x (caar rules))))))
            (else (loop (cdr rules))))))

  ;; Simplified version of syntax case. We need it to bootstrap, when
  ;; lgears will be able to compile itself, the code using
  ;; syntax-match will be rewriten with syntax-case
  (define-syntax syntax-match
    (lambda (x)
      (define (rename-args args)
        (map (lambda (a)
               (string->symbol
                (string-append "_" (symbol->string a))))
             args))
      (syntax-case x ()
        ((_ source reserved . fields)
         (let* ((fields* (syntax->datum #'fields)))
           #`(syntax-dispatch
                source
                'reserved
                #,@(map (lambda (f)
                          `(cons ',(car f)
                                 (lambda ,(rename-args (cdar f))
                                   ,(cadr f))))
                        fields*)))))))

  (define (exp-quote x r mr)
    (syntax-match
     x ()
     ((quote a) `(quote ,(strip _a)))))

  (define (make-lambda-env env labels new-vars)
    (fold-left (lambda (prev l v)
                 (extend-env l (make-binding 'lexical v) prev))
               env labels new-vars))

  (define (exp-lambda x r mr)
    (let* ((vars (syntax->list (syntax-cadr x)))
           (body (syntax-cddr x))
           (new-vars (map (lambda (x)
                            (gen-name (syntax-object-expr x)))
                          vars))
           (labels (map (lambda (x) (make-label)) vars))
           (env (make-lambda-env r labels new-vars)))
      `(lambda ,new-vars
         ,@(exp-dispatch (fold-left (lambda (xpr id label)
                                      (add-subst id label xpr))
                                    body vars labels)
                         env mr))))
  
  (define (exp-if x r mr)
    (syntax-match
     x ()
     ((if a b)
      `(if ,(exp-dispatch _a r mr)
           ,(exp-dispatch _b r mr)))
     ((if a b c)
      `(if ,(exp-dispatch _a r mr)
           ,(exp-dispatch _b r mr)
           ,(exp-dispatch _c r mr)))))

  (define (exp-set! x r mr)
    (syntax-match
     x ()
     ((set! a b)
      `(set! ,(exp-dispatch _a r mr)
             ,(exp-dispatch _b r mr)))))

  (define (macro-or x)
    (syntax-match
     x ()
     ((or) #f)
     ((or a) _a)
     ((or a b)
      `(let ((t ,_a))
         (if t t ,_b)))))

  (define (macro-let x)
    (syntax-match
     x ()
     ((let vars body)
      (let ((args (syntax->list _vars)))
        `((lambda ,(map syntax-car args)
            ,_body)
          ,@(map syntax-cadr args))))))

  (define (initial-wrap-end-env)
    (define bindings
      `((quote . ,(make-binding 'core exp-quote))
        (lambda . ,(make-binding 'core exp-lambda))
        (if . ,(make-binding 'core exp-if))
        (set! . ,(make-binding 'core exp-set!))
        (let . ,(make-binding 'macro macro-let))
        (or . ,(make-binding 'macro macro-or))
        ))
    (let ((labels (map (lambda (x) (make-label)) bindings)))
      (values
       `(,@(map (lambda (sym label)
                  (make-subst sym (list top-mark) label))
                (map car bindings)
                labels)
         ,top-mark)
       (map cons labels (map cdr bindings)))))

  (define (expand x)
    (let-values (((wrap env) (initial-wrap-end-env)))
      (exp-dispatch (make-syntax-object x wrap) env env)))

  )
