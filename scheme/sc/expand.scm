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
  (export expand)
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

  (define (syntax-dispatch x reserved . rules)
    (let loop ((rules rules))
      (cond ((null? rules)
             (syntax-error x "invalid syntax, no match"))
            ((pattern-match reserved x (caar rules))
             => (lambda (res)
                  (format #t "matched ~a = ~a\n" (caar rules) (strip res))
                  (apply (cdar rules)
                         (cdr (pattern-bind res (caar rules) '())))))
            (else (loop (cdr rules))))))

  ;; Simplified version of syntax case. We need it to bootstrap, when
  ;; lgears will be able to compile itself, the code using
  ;; syntax-match will be rewriten with syntax-case
  (define-syntax syntax-match
    (lambda (x)
      ;; Extract variables from pattern
      (define (get-vars v)
        (cond ((null? v) '())
              ((pair? v)
               (let ((l (get-vars (car v)))
                     (r (get-vars (cdr v))))
                 (if l
                     (if (pair? l)
                         (append l r)
                         (cons l r))
                     r)))
              ((eq? v '_) #f)
              ((eq? v '...) #f)
              ((symbol? v) v)
              (else #f)))
      (syntax-case x ()
        ((_ source reserved . fields)
         (let* ((fields* (syntax->datum #'fields)))
           #`(syntax-dispatch
                source
                'reserved
                #,@(map (lambda (f)
                          `(cons ',(car f)
                                 (lambda ,(get-vars (cdar f))
                                   ,(cadr f))))
                        fields*)))))))

  (define (exp-quote x r mr)
    (syntax-match
     x ()
     ((quote a) `(quote ,(strip a)))))

  (define (make-lambda-env env labels new-vars)
    (fold-left (lambda (prev l v)
                 (extend-env l (make-binding 'lexical v) prev))
               env labels new-vars))

  (define (scan-defines body)
    (let ((defines (filter (lambda (x)
                             (and (syntax-pair? x)
                                  (eq? (syntax-object-expr (syntax-car x))
                                       'define)))
                           body)))
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

  (define (exp-lambda x r mr)
    (syntax-match
     x ()
     ((lambda (varlist ...) body ...)
      (let* ((new-vars (gen-names varlist))
             (defines (scan-defines body))
             (new-defines (gen-names defines))
             (env-vars (append varlist defines))
             (labels (gen-labels env-vars))
             (env (make-lambda-env
                   r labels (append new-vars new-defines))))
        `(lambda ,new-vars
           ,@(exp-dispatch (fold-left (lambda (xpr id label)
                                        (add-subst id label xpr))
                                      body env-vars labels)
                           env mr))))))

  (define (exp-define x r mr)
    (syntax-match
      x ()
      ((define (var args ...) body ...)
       `(define ,(exp-dispatch var r mr)
          ,(exp-dispatch (extend-wrap
                           (syntax-object-wrap x)
                           `(lambda ,args
                              ,@body))
                         r mr)))
      ((define var val)
       `(define ,(exp-dispatch var r mr)
          ,(exp-dispatch val r mr)))
      ((define var)
       `(define ,(exp-dispatch var r mr) (void)))))
  
  (define (exp-if x r mr)
    (syntax-match
     x ()
     ((if a b)
      `(if ,(exp-dispatch a r mr)
           ,(exp-dispatch b r mr)
           (void)))
     ((if a b c)
      `(if ,(exp-dispatch a r mr)
           ,(exp-dispatch b r mr)
           ,(exp-dispatch c r mr)))))

  (define (exp-set! x r mr)
    (syntax-match
     x ()
     ((set! a b)
      `(set! ,(exp-dispatch a r mr)
             ,(exp-dispatch b r mr)))))

  (define (macro-or x)
    (syntax-match
     x ()
     ((or) #f)
     ((or a) a)
     ((or a b ...)
      (if (or (identifier? a)
              (self-evaluating? (strip a)))
          `(if ,a ,a (or ,@b))
      `(let ((t ,a))
         (if t t (or ,@b)))))))

  (define (macro-and x)
    (syntax-match
     x ()
     ((and) #t)
     ((and a) a)
     ((and a b ...)
      `(if ,a (and ,@b)))))
  
  (define (macro-let x)
    (syntax-match
     x ()
     ((let ((vars vals) ...) e1 e2 ...)
      `((lambda ,vars
          ,e1 ,@e2)
        ,@vals))
     ((let loop ((vars vals) ...) e1 e2 ...)
      `(let ((,loop 'unspec))
         (set! ,loop (lambda ,vars ,e1 ,@e2))
         (,loop ,@vals)))))

  (define (initial-wrap-end-env)
    (define bindings
      `((quote . ,(make-binding 'core exp-quote))
        (lambda . ,(make-binding 'core exp-lambda))
        (define . ,(make-binding 'core exp-define))
        (if . ,(make-binding 'core exp-if))
        (set! . ,(make-binding 'core exp-set!))
        (let . ,(make-binding 'macro macro-let))
        (or . ,(make-binding 'macro macro-or))
        (and . ,(make-binding 'macro macro-and))
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
      (exp-dispatch (make-syntax-object x wrap) env env)))

  (pretty-print (expand '(let iter ((t 12) (a 40)) (or (t a) 10 (iter 12)))))
  (pretty-print (expand '(lambda (x y) (define foo) (define (bar x) (y x)))))
  )
