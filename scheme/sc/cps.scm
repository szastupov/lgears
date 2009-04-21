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

(library (sc cps)
  (export cps-convert)
  (import (rnrs)
          (only (core) pretty-print) ; This works only for ypsilon
          (sc gen-name)
          (format))

  (define (self-eval? node)
    (or (not (pair? node))
        (and (eq? (car node) 'quote)
             (not (or (null? (cdr node))
                      (pair? (cadr node))
                      (vector? (cadr node)))))))

  (define (inlinable? node)
    (and (pair? node)
         (eq? (car node) 'lambda)))

  (define (trquote qv)
    (define (apply-pfunc x)
      (cond ((pair? x)
             (trquote x))
            ((symbol? x)
             `(quote ,x))
            ((null? x)
             ''())
            (else x)))

    (define (pairs->list p)
      (if (pair? p)
          (cons (car p) (pairs->list (cdr p)))
          (cons p '())))

    (cond ((list? qv)
           (cons 'list (map apply-pfunc qv)))
          ((null? qv)
           '())
          ((pair? qv)
           (cons 'cons* (map apply-pfunc (pairs->list qv))))
          (else (apply-pfunc qv))))

  (define (convert-quote res node name)
    (cond ((pair? node)
           (convert res (trquote node) name))
          ((vector? node)
           (convert res (cons 'vector (vector->list node)) name))
          (else node)))

  ;; Functions like set! may introduce begin forms, just splice it
  (define (splice-begin body)
    (define (begin? x)
      (and (pair? x)
           (eq? (car x) 'begin)))
    (fold-right (lambda (x y)
                  (if (begin? x)
                      (append (splice-begin (cdr x)) y)
                      (cons x y)))
                '() body))

  (define (convert-func args body)
    (let ((name (gen-name)))
      `(lambda ,(cons name args)
         ,@(splice-begin (convert-body body name)))))

  (define (convert-lambda node)
    (convert-func (cadr node) (cddr node)))

  (define (convert-set res node name)
    (define (set-body val)
      `(begin
         (set! ,(car node) ,val)
         ,(if (null? res)
              `(void ,name)
              res)))
    (let ((value (cadr node)))
      (cond ((self-eval? value)
             (set-body value))
            ((inlinable? value)
             (set-body (convert-lambda value)))
            (else
             (let ((setter (gen-name))
                   (nv (gen-name)))
               `((lambda (,setter)
                   ,(convert '() value setter))
                 (lambda (,nv)
                   ,(set-body nv))))))))

  (define (convert res node name)
    (if (self-eval? node)
      (if (null? res)
        (list name node)
        `((lambda (,name) ,res) ,node))

      (case (car node)
        ((lambda)
         (let ((func (convert-lambda node)))
           (if (null? res)
             (list name func)
             `((lambda (,name) ,res) ,func))))

        ((if)
         (let* ((args (cdr node))
                (pred (car args))
                (predname (if (self-eval? pred)
                            pred (gen-name)))
                (lname (gen-name))
                (escape (if (null? res) name lname))
                (condition `(if ,predname
                              ,(convert '() (cadr args) escape)
                              ,(convert '() (caddr args) escape)))
                (expr (if (null? res)
                        condition
                        `((lambda (,lname) ,condition)
                          (lambda (,name) ,res)))))
           (if (self-eval? pred)
             expr
             (convert expr pred predname))))

        ((begin)
         (convert-seq res (cdr node) name))

        ((set!)
         (convert-set res (cdr node) name))

        ((define)
         (syntax-violation 'convert "misplaced defination" node))

        ((quote)
         (convert-quote res (cadr node) name))

        ;;; Conversion of call
        ;;; We covnert both arguments and functions
        ;;; FIXME function fail if symbol in tail position
        (else
          (let* ((args (reverse node))
                 ;;; List of arguments ready for applicaton
                 (largs (map (lambda (x)
                               (cond ((self-eval? x) x)
                                     ((inlinable? x) (convert-lambda x))
                                     (else (gen-name))))
                             args))
                 (rlargs (reverse largs))
                 (control (cond ((null? res)
                                 name)
                                ((and (pair? (car res))
                                      (eq? (caar res) 'set!)) ;;FIXME
                                 `(lambda (,name) ,@res))
                                (else
                                  `(lambda (,name) ,res))))
                 (expr `(,(car rlargs) ,control ,@(cdr rlargs))))
            ;;; Clue everything
            (fold-left (lambda (prev x n)
                         (if (or (self-eval? x) (inlinable? x))
                           prev
                           (convert prev x n)))
                       expr args largs))))))

  (define (convert-seq res source name)
    (if (null? source)
      '()
      (let* ((seq (reverse source))
             (frst (car seq)))
        (define (tail? x)
          (eq? x frst))
        (fold-left (lambda (prev x)
                     (if (self-eval? x)
                         ;; As self-evals does not cause side effects
                         ;; they are useless outside of tail context
                         (if (tail? x)
                             (list name x)
                             prev)
                     (convert prev x (if (tail? x)
                                       name
                                       (gen-name)))))
                   res seq))))

  (define (defination? x)
    (and (pair? x) (eq? (car x) 'define)))

  (define (convert-body body name)
    (let-values (((defines expressions)
                  (partition defination? body)))
      (if (null? expressions)
          (syntax-violation 'convert-body "empty body" #f))
      (let* ((rest (convert-seq '() expressions name))
             (extend (if (null? defines)
                         '()
                         `((extend ,(map (lambda (x)
                                           (if (pair? (cadr x))
                                               (caadr x)
                                               (cadr x)))
                                         defines))))))
        `(,@extend
          ,(fold-left (lambda (prev x)
                        (if (null? (cddr x))
                            prev
                            (convert-set prev (cdr x) (gen-name))))
                      rest (reverse defines))))))

  (define (cps-convert source)
    (let ((res (convert-lambda source)))
      (list res '__exit)))

  ;(pretty-print (cps-convert '(
                               ;(set! foo (lambda (x y) (x y)))
                               ;(display foo)
                               ;)))
  )
