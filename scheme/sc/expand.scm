#|
 | Copyright (C) 2009 - Stepan Zastupov
 | This program is free software; you can redistribute it and/or
 | modify it under the terms of the GNU General Public License
 | as published by the Free Software Foundation; either version 2
 | of the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with this program; if not, write to the Free Software
 | Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 |#

(library (sc expand)
  (export expand expand-top sc-dispatch gen-syntax syntax-error
          (rename (strip syntax->datum)))
  (import (except (rnrs) identifier? ...)
          (rnrs eval)
          (format)
          (sc gen-name)
          (sc syntax-core)
          (sc syntax-pattern)
          (only (core) pretty-print))
  
  (define (exp-macro p x)
    (let* ((m (make-mark))
           (xm (add-mark m x)))
      (add-mark m (extend-wrap
                   (syntax-object-wrap xm)
                   (p xm)))))

  (define (exp-core p x r)
    (p x r))

  (define (exp-exprs x* r)
    (map (lambda (x)
           (exp-dispatch x r))
         x*))

  (define (exp-dispatch x r)
    (cond ((identifier? x)
           (cond ((id-binding x r)
                  => (lambda (b)
                       (case (binding-type b)
                         ((macro) (exp-dispatch (exp-macro (binding-value b) x) r))
                         ((lexical)(binding-value b))
                         (else (syntax-error x "invalid syntax")))))
                 (else (strip x))))
          ((not (syntax-pair? x))
           (strip x))
          ((identifier? (syntax-car x))
           (cond ((id-binding (syntax-car x) r)
                  => (lambda (b)
                       (case (binding-type b)
                         ((macro) (exp-dispatch (exp-macro (binding-value b) x) r))
                         ((lexical)
                          `(,(binding-value b)
                            ,@(exp-exprs (syntax->list (syntax-cdr x)) r)))
                         ((core) (exp-core (binding-value b) x r))
                         (else (syntax-error x "invalid syntax")))))
                 (else
                  `(,(strip (syntax-car x))
                    ,@(exp-exprs (syntax->list (syntax-cdr x)) r)))))
          (else
           `(,(exp-dispatch (syntax-car x) r)
             ,@(exp-exprs (syntax->list (syntax-cdr x)) r)))))

  (define (syntax-dispatch x reserved . rules)
    (let loop ((rules rules))
      (cond ((null? rules)
             (syntax-error x "invalid syntax, no match"))
            ((pattern-match reserved x (caar rules))
             => (lambda (res)
                  ;(format #t "matched ~a = ~a\n" (caar rules) (strip res))
                  (apply (cdar rules)
                         (cdr (pattern-bind res (caar rules) '())))))
            (else (loop (cdr rules))))))

  (define (gen-syntax vars stx)
    (define (ellipsis-pair? x)
      (and (pair? x)
           (pair? (cdr x))
           (eq? (cadr x) '...)))
    (let rewrite ((stx stx))
      (cond ((ellipsis-pair? stx)
             (cond ((assq (car stx) vars) => cdr)
                   (else (syntax-error stx "ellipsis after non-patern"))))
            ((pair? stx)
             (cons (rewrite (car stx))
                   (rewrite (cdr stx))))
            ((symbol? stx)
             (cond ((assq stx vars) => cdr)
                   (else stx)))
            (else stx))))

  (define (sc-dispatch x reserved . rules)
    (let loop ((rules rules))
      (cond ((null? rules)
             (syntax-error x "invalid syntax, no match"))
            ((pattern-match reserved x (caar rules))
             => (lambda (matched)
                  (let* ((bind (pattern-bind-named
                                matched (caar rules) '()))
                         (res ((cdar rules) bind)))
                    ;(format #t "bind ~a\n" (strip bind))
                    res)))
            (else (loop (cdr rules))))))

  ;; Simplified version of syntax case. We need it to bootstrap, when
  ;; lgears will be able to compile itself, the code using
  ;; syntax-match will be rewriten with syntax-case
  (define-syntax syntax-match
    (lambda (x)
      ;; Extract variables from pattern
      (define (append* l r)
        (let loop ((l l))
          (cond ((null? l) r)
                ((pair? l)
                 (cons (car l) (loop (cdr l))))
                (else
                 (cons l r)))))

      (define (get-vars v)
        (cond ((null? v) '())
              ((pair? v)
               (let ((l (get-vars (car v)))
                     (r (get-vars (cdr v))))
                 (if l
                     (if (pair? l)
                         (append* l r)
                         (cons l r))
                     r)))
              ((eq? v '_) #f)
              ((eq? v '...) #f)
              ((symbol? v) v)
              (else #f)))

      (syntax-case x ()
        ((_ source reserved fields ...)
         (let ((fields*  (syntax->datum #'(fields ...))))
           #`(syntax-dispatch
                source
                'reserved
                #,@(map (lambda (f)
                          (datum->syntax #'source
                          `(cons ',(car f)
                                 (lambda ,(get-vars (cdar f))
                                   ,(cadr f)))))
                        fields*)))))))

  (define (exp-quote x r)
    (syntax-match
     x ()
     ((quote a) `(quote ,(strip a)))))

  (define (scan-defines body)
    (define (define? x)
      (and (syntax-pair? x)
           (eq? (syntax-object-expr (syntax-car x))
                'define)))
    (let ((defines (filter define? body)))
      (map (lambda (x)
             (let ((head (syntax-cadr x)))
               (if (syntax-pair? head)
                   (syntax-car head)
                   head)))
           defines)))

  (define (gen-names lst)
    (map (lambda (x)
           (gen-name (syntax-object-expr x)))
         lst))

  (define (gen-labels lst)
    (map (lambda (x) (make-label)) lst))

  (define (improper->propper pair)
    (let loop ((cur pair))
      (cond ((pair? cur)
             (cons (car cur) (loop (cdr cur))))
            (else (list cur)))))

  (define (splice-begin body)
    (define (begin? x)
      (and (syntax-pair? x)
           (eq? (syntax-object-expr
                 (syntax-car x))
                'begin)))
    (fold-right (lambda (x y)
                  (if (begin? x)
                      (append (splice-begin
                               (syntax->list
                                (syntax-cdr x)))
                              y)
                      (cons x y)))
               '() body))

  (define (extend-syntax env body)
    (define (define-syntax? x)
        (and (syntax-pair? x)
             (eq? (syntax-object-expr (syntax-car x))
                  'define-syntax)))
    
    (define (make-macro macro)
      (syntax-match
       macro ()
       ((define-syntax name expander)
        (list (strip name)
              name
              (eval (exp-dispatch
                     expander
                     env)
                    (environment '(except (rnrs) syntax->datum)
                                 '(sc expand)))))))
    
    (let-values (((macro* expr*) (partition define-syntax? body)))
      (let* ((compiled (map make-macro macro*)))
        (values
         expr*
          (map (lambda (macro)
                 (cons (car macro)
                        (syntax-object-wrap (cadr macro))))
               compiled)
          (map (lambda (macro)
                 (make-binding 'macro (caddr macro)))
               compiled)))))

  (define (exp-lambda x env)
    (define (expand-lambda varlist new-vars body)
      (let-values (((body macro-subst macro-bindindings)
                          (extend-syntax env (splice-begin body))))
        
        (let* ((defines (scan-defines body))
               (new-defines (gen-names defines))
               (env-vars (append varlist defines))
               (lexical-bindings (map (lambda (v)
                                        (make-binding 'lexical v))
                                      (append new-vars new-defines)))
               (lexical-subst (map (lambda (id)
                                     (cons
                                      (syntax-object-expr id)
                                      (syntax-object-wrap id)))
                                   env-vars))
               (all-bindings (append macro-bindindings
                                     lexical-bindings))
               (all-subst (append macro-subst lexical-subst))
               (labels (gen-labels all-bindings)))
          (exp-dispatch 
           (extend-wrap
            (map (lambda (id label)
                   (make-subst (car id)
                               (wrap-marks (cdr id))
                               label))
                 all-subst labels)
            body)
           (fold-left (lambda (prev l v)
                        (extend-env l v prev))
                      env labels all-bindings)))))

    (syntax-match
     x ()
     ((lambda (varlist ...) body ...)
      (let ((new-vars (gen-names varlist)))
        `(lambda ,new-vars
           ,@(expand-lambda varlist new-vars body))))
     
     ((lambda (v1 . rem) body ...)
      (let* ((varlist (improper->propper
                       (cons v1 (syntax->pair rem))))
             (new-vars (gen-names varlist)))
        `(lambda ,(apply cons* new-vars)
           ,@(expand-lambda varlist new-vars body))))
     
     ((lambda var body ...)
      (let* ((varlist (list var))
             (new-vars (gen-names varlist)))
        `(lambda ,@new-vars
           ,@(expand-lambda varlist new-vars body))))))

  (define (exp-define x r)
    (syntax-match
      x ()
      ((define (var args ...) body ...)
       `(define ,(exp-dispatch var r)
          ,(exp-dispatch (extend-wrap
                           (syntax-object-wrap x)
                           `(lambda ,args
                              ,@body))
                         r)))
      ((define var val)
       `(define ,(exp-dispatch var r)
          ,(exp-dispatch val r)))
      ((define var)
       `(define ,(exp-dispatch var r) (void)))))
  
  (define (exp-if x r)
    (syntax-match
     x ()
     ((if a b)
      `(if ,(exp-dispatch a r)
           ,(exp-dispatch b r)
           (void)))
     ((if a b c)
      `(if ,(exp-dispatch a r)
           ,(exp-dispatch b r)
           ,(exp-dispatch c r)))))

  (define (exp-set! x r)
    (syntax-match
     x ()
     ((set! a b)
      `(set! ,(exp-dispatch a r)
             ,(exp-dispatch b r)))))

  (define (exp-begin x r)
    (syntax-match
     x ()
     ((begin e)
      (exp-dispatch e r))
     ((begin body ...)
      `(begin ,@(exp-exprs body r)))))

  (define (exp-syntax x r)
    (syntax-match
     x ()
     ((syntax e) `(gen-syntax vars ',(strip e)))))

  (define (exp-syntax-case x r)
    (syntax-match
     x ()
     ((syntax-case src reserved (pat* acc*) ...)
      `(sc-dispatch
        ,(exp-dispatch src r)
        ',(strip reserved)
        ,@(map (lambda (pat acc)
                 `(cons ',(strip pat)
                        (lambda (vars)
                          ,(exp-dispatch acc r))))
               pat* acc*)))))

  (define (initial-wrap-end-env)
    (define bindings
      `((quote . ,(make-binding 'core exp-quote))
        (lambda . ,(make-binding 'core exp-lambda))
        (define . ,(make-binding 'core exp-define))
        (if . ,(make-binding 'core exp-if))
        (set! . ,(make-binding 'core exp-set!))
        (begin . ,(make-binding 'core exp-begin))
        (syntax . ,(make-binding 'core exp-syntax))
        (syntax-case . ,(make-binding 'core exp-syntax-case))
        ))
    (let ((labels (gen-labels bindings)))
      (values
       `(,@(map (lambda (sym label)
                  (make-subst sym (list top-mark) label))
                (map car bindings)
                labels)
         ,top-mark)
       (map cons labels (map cdr bindings)))))

  (define (expand x)
    (let-values (((wrap env) (initial-wrap-end-env)))
      (exp-dispatch (make-syntax-object x wrap) env)))

  (define (expand-top x)
    (expand `(lambda () ,@x)))

  )
