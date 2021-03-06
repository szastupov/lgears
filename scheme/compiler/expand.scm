#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
 |
 | This program is free software; you can redistribute it and/or
 | modify it under the terms of the GNU Lesser General Public
 | License as published by the Free Software Foundation; either
 | version 3 of the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 | Lesser General Public License for more details.
 |
 | You should have received a copy of the GNU Lesser General
 | Public Licens along with this program, if not; see
 | <http://www.gnu.org/licenses>.
 |#

(library (compiler expand)
  (export expand-file sc-dispatch gen-syntax syntax-error syntax-vars)
  (import (rnrs eval)
		  (rename (rnrs)
				  (identifier? sys-identifier?)
				  (syntax->datum sys-syntax->datum)
				  (datum->syntax sys-datum->syntax))
		  (format)
		  (config)
		  (compiler gen-name)                 ; Fresh name generator
		  (compiler syntax-core)              ; Core routines
		  (compiler syntax-pattern)           ; Pattern matching
		  (compiler library-manager)          ; Library managment
		  (compiler compiler)
		  (compiler cps)
		  (compiler fasl)
          (compiler source-optimizer)
		  (only (core) pretty-print))

  (define syntax-vars '())

  (define libraries-root (library-manager-root))

  (define (exp-macro p x)
    (let* ((m (make-mark))
           (xm (add-mark m x)))
      (add-mark m (extend-wrap-from xm (p xm)))))

  (define (exp-core p x r)
	(p x r))

  (define (exp-exprs x* r)
	(let loop ((x x*))
	  (cond ((null? x)
			 '())
			((pair? x)
			 (cons (expand (car x) r)
				   (loop (cdr x))))
			(else (expand x r)))))

  (define (expand x r)
	(cond ((identifier? x)
		   (cond ((id-binding x r)
				  => (lambda (b)
					   (case (binding-type b)
						 ((macro)
						  (expand (exp-macro (binding-value b) x) r))
						 ((lexical)
						  (binding-value b))
						 (else (syntax-error x "invalid syntax")))))
				 (else (strip x))))
		  ((not (syntax-pair? x))
		   (strip x))
		  ((identifier? (syntax-car x))
		   (cond ((id-binding (syntax-car x) r)
				  => (lambda (b)
					   (case (binding-type b)
						 ((macro)
						  (expand (exp-macro (binding-value b) x) r))
						 ((lexical)
						  (cons (binding-value b)
								(exp-exprs (syntax->pair (syntax-cdr x)) r)))
						 ((core)
						  (exp-core (binding-value b) x r))
						 (else (syntax-error x "invalid syntax")))))
				 (else
				  (cons (strip (syntax-car x))
						(exp-exprs (syntax->pair (syntax-cdr x)) r)))))
		  (else
		   (cons (expand (syntax-car x) r)
				 (exp-exprs (syntax->pair (syntax-cdr x)) r)))))

  (define (syntax-dispatch x reserved . rules)
	(let loop ((rule rules))
	  (cond ((null? rule)
			 (syntax-error
			  x "invalid syntax, no match"))
			((pattern-match reserved x (caar rule))
			 => (lambda (res)
				  ;(format #t "matched ~a = ~a\n" (caar rule) (strip res))
				  (apply (cdar rule)
						 (pattern-bind res (caar rule) '()))))
			(else (loop (cdr rule))))))

  (define (gen-syntax vars stx)
	(define (ellipsis-pair? x)
	  (and (pair? x)
		   (pair? (cdr x))
		   (eq? (cadr x) '...)))
    ;(format #t "vars: ~s\n" (strip vars))
	(let rewrite ((stx stx))
	  (cond ((ellipsis-pair? stx)
			 (cond ((and (symbol? (car stx))
                         (assq (car stx) vars))
					=> (lambda (res)
						 (if (and (= (length (cdr res)) 1)
								  (eq? (cadr res) '()))
							 '()
							 (cdr res))))
				   (else stx)))
			((pair? stx)
			 (cons (rewrite (car stx))
				   (rewrite (cdr stx))))
			((symbol? stx)
			 (cond ((assq stx vars) => cdr)
				   (else stx)))
			(else stx))))

  (define (pass-fender? variant bind)
    (let ((fender (cadr variant)))
      (if fender
          (fender bind)
          #t)))

  (define (sc-dispatch vars x reserved . rules)
	(let loop ((rule rules))
	  (cond ((null? rule)
			 (syntax-error
			  x "invalid syntax, no match"))
			((pattern-match reserved x (caar rule))
			 => (lambda (matched)
				  (let* ((bind (pattern-bind-named reserved
                                                   matched
                                                   (caar rule)
                                                   vars)))
                    (if (pass-fender? (car rule) bind)
                        ((caddar rule) bind)
                        (loop (cdr rule))))))
			(else (loop (cdr rule))))))

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
		 (let ((fields*  (sys-syntax->datum #'(fields ...))))
		   #`(syntax-dispatch
				source
				'reserved
				#,@(map (lambda (f)
						  (sys-datum->syntax #'source
						  `(cons ',(car f)
								 (lambda ,(get-vars (car f))
								   ,(cadr f)))))
						fields*)))))))

  (define (exp-quote x r)
	(syntax-match
	 x ()
	 ((_ a) `(quote ,(strip a)))))

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

  ;; Syntax-specifinc splice-begin (we have same for plain lists in cps)
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

  (define sys-env '((except (rnrs)
                            syntax->datum
                            syntax-case
                            syntax-rules
                            identifier?
                            datum->syntax
                            generate-temporaries)
                    ;; For compatibility
                    (rename (rnrs base)
                            (cons $cons))
                    (compiler expand)
                    (compiler syntax-core)))

  (define (make-macro macro env)
    (syntax-match
     macro ()
     ((_ name expander)
      (cons name
            (eval (expand expander env)
                  (apply environment sys-env))))))

  ;; Sequentional macro compilation
  (define (compile-i wrap env macro*)
	(if (null? macro*)
		(values wrap env)
		(let* ((compiled (make-macro (extend-wrap wrap (car macro*)) env))
			   (label (make-label))
			   (subst (add-subst (car compiled) label))
			   (binding (macro-binding (cdr compiled))))
		  (compile-i (cons subst wrap)
					 (extend-env label binding env)
                     (cdr macro*)))))

  (define (extend-syntax env body)
	(define (define-syntax? x)
	  (and (syntax-pair? x)
		   (eq? (syntax-object-expr (syntax-car x))
				'define-syntax)))
	(let*-values (((macro body) (partition define-syntax? body))
                  ((wrap env) (compile-i '() env macro)))
      (values body env wrap)))

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

  (define (expand-body src env varlist new-vars body)
	(let-values (((body env wrap)
				  (extend-syntax env (splice-begin body))))
	  (let* ((defines (scan-defines body))
			 (new-defines (gen-names defines))
			 (env-vars (append varlist defines))
			 (labels (gen-labels env-vars))
             (marks (wrap-marks (syntax-object-wrap src))))
        (define (new-subst v l)
          (make-subst (syntax-object-expr v) marks l))
		(expand
		 (extend-wrap
          (append (map new-subst env-vars labels)
                  wrap)
		  body)
		 (fold-left (lambda (prev label v)
					  (extend-env label
								  (lexical-binding v)
								  prev))
					env labels
					(append new-vars new-defines))))))

  (define (exp-lambda x env)
	(define (expand-lambda varlist new-vars body)
	  (expand-body x env varlist new-vars body))
	(syntax-match
	 x ()
	 ((_ (varlist ...) body ...)
	  (let ((new-vars (gen-names varlist)))
		`(lambda ,new-vars
		   ,@(expand-lambda varlist new-vars body))))

	 ((_ (v1 . rem) body ...)
	  (let* ((varlist (improper->propper
					   (cons v1 (syntax->pair rem))))
			 (new-vars (gen-names varlist)))
		`(lambda ,(apply cons* new-vars)
		   ,@(expand-lambda varlist new-vars body))))

	 ((_ var body ...)
	  (let* ((varlist (list var))
			 (new-vars (gen-names varlist)))
		`(lambda ,@new-vars
		   ,@(expand-lambda varlist new-vars body))))))

  (define (exp-define x r)
	(syntax-match
	 x ()
	 ((_ (var args ...) body ...)
	  `(define ,(expand var r)
		 ,(expand (extend-wrap-from
				   x
				   `(lambda ,args
					  ,@body))
				  r)))

	 ((_ (var . rem) body ...)
	  `(define ,(expand var r)
		 ,(expand (extend-wrap-from
						 x
						 `(lambda ,rem
							,@body))
						r)))

	 ((_ var val)
	  `(define ,(expand var r)
		 ,(expand val r)))

	 ((_ var)
	  `(define ,(expand var r) (void)))))

  (define (exp-if x r)

	(define (always-true? dt)
      (and dt
           (not (pair? dt))
           (not (symbol? dt))))
	(define (always-false? dt)
      (not dt))

	(syntax-match
	 x ()
	 ((_ a b)
	  (expand (extend-wrap-from
			   x `(if ,a ,b (void))) r))
	 ((_ a b c)
      (let ((pred (expand a r)))
        (cond ((always-true? pred) (expand b r))
              ((always-false? pred) (expand c r))
              (else
               `(if ,pred
                    ,(expand b r)
                    ,(expand c r))))))))

  (define (exp-set! x r)
	(syntax-match
	 x ()
	 ((_ a b)
	  `(set! ,(expand a r)
			 ,(expand b r)))))

  (define (exp-begin x r)
	(syntax-match
	 x ()
	 ((_ e)
	  (expand e r))
	 ((_ body ...)
	  `(begin ,@(exp-exprs body r)))))

  (define (exp-builtin x r)
    (syntax-match
     x ()
     ((_ func argl ...)
      (begin
        (if (not (identifier? func))
            (syntax-error func "expected identifier"))
        `(builtin-call ,(strip func) ,@(exp-exprs argl r))))))

  (define (exp-syntax x r)
	(syntax-match
	 x ()
	 ((_ e) `(gen-syntax syntax-vars ',(strip e)))))

  (define (exp-syntax-case x r)
    (define (make-variant var)
      (syntax-match
       var ()
       ((pat acc)
        (expand (datum->syntax
                 x `(list ',(strip pat)
                          #f
                          (lambda (syntax-vars)
                            ,(expand acc r))))
                r))
       ((pat fender acc)
        (expand (datum->syntax
                 x `(list ',(strip pat)
                          (lambda (syntax-vars)
                            ,(expand fender r))
                          (lambda (syntax-vars)
                            ,(expand acc r))))
                r))))
	(syntax-match
	 x ()
	 ((_ src reserved variants ...)
	  `(sc-dispatch
        syntax-vars
		,(expand src r)
		',(strip reserved)
		,@(map make-variant variants)))))

  (define (load-library name)
	(let ((path (find-library-file name)))
	  (format #t ";; loading library ~a\n" path)
	  (let ((expanded  (expand-file path)))
		(assemble
		 (start-compile
		  (cps-convert expanded))
		 (format "~a/~a.o" (get-cache-path) path)))
      (format #t ";; loaded\n")
	  (find-library libraries-root name)))

  (define (resolve-imports imports)
	(define (resolve-import name)
	  (cond ((find-library libraries-root name)
			 => (lambda (res) res))
			(else (load-library name))))
	(let ((resolved (map resolve-import imports)))
	  ;; It's a bit messy because we need to merge all bindings in a
	  ;; single list
	  (values
	   (fold-left (lambda (pimp cur)
					(fold-left (lambda (prev exprt)
								 (cons (make-subst
										(car exprt)
										(list top-mark)
										(cddr exprt))
									   prev)) pimp (cdr cur)))
				  (list top-mark) resolved)
	   (fold-left (lambda (pimp cur)
					(fold-left (lambda (prev exprt)
								 (cons (cons (cddr exprt)
											 (cadr exprt))
									   prev)) pimp (cdr cur)))
				  '() resolved)
	   (apply append (map car resolved)))))

  (define (make-body body wrap)
	(syntax->list
	 (make-syntax-object body wrap)))

  (define (expand-top x)
	(if (not (eq? (caar x) 'import))
		(error 'expand-top "expected import in top-level"))
	(let-values (((wrap env include) (resolve-imports (cdar x))))
	  (let ((body (append include (cdr x))))
        (optimize-source
         (expand
          (make-syntax-object `(lambda () ,@body) wrap)
          env)))))

  (define (make-exporter name defines)
	(if (null? defines)
		'((void))
		`((define exports
			(list ,@(map (lambda (x)
						   `(cons ',x ,x))
						 defines)))
          (let ((res (lambda (imp) (__assqd? imp exports))))
            (library-cache (cons (cons ',name res)
                                 (library-cache)))
            res))))

  (define (make-include name defines)
	(let ((name (libname->symbol name)))
	  `((define ,name (load-library ',name))
		,@(map (lambda (x)
                 `(define ,x (,name ',x)))
			   defines))))

  (define (expand-library x)
	(define (get-exports)
	  (let ((res (caddr x)))
		(if (eq? (car res) 'export)
			(cdr res)
			(error 'expand-library "expected export form"))))

	(define (get-imports)
	  (let ((res (cadddr x)))
		(if (eq? (car res) 'import)
			(cdr res)
			(error 'expand-library "expected import form"))))

	(let ((name (cadr x))
		  (exports (get-exports))
		  (imports (get-imports))
		  (body (cddddr x)))

	  (let*-values (((init-wrap init-env include)
					 (resolve-imports imports))
					((body env wrap)
					 (extend-syntax init-env (make-body body init-wrap))))

		(let ((wrap (append wrap init-wrap))
			  (defines (strip (scan-defines body))))

		  (define (extract-binding exprt)
			(let ((se (strip exprt)))
			  (cons* se
					 (label-binding exprt (find-label se wrap) env)
					 (make-label))))

		  ;; separate exported defines and macros
		  (let-values (((defines macros)
						(partition (lambda (x)
									 (memq x defines))
								   exports)))
			;; Install exports
			(library-install! libraries-root
							  name
							  (cons (make-include name defines)
									(map extract-binding macros)))
			;; Produce expanded library
            (optimize-source
             (expand (make-syntax-object
                      (let ((name (libname->symbol name)))
                        `(lambda ()
                           (define (__assqd? x lst)
                             (let loop ((cur lst))
                               (cond ((null? cur) #f)
                                     ((eq? (car (car cur)) x)
                                      (cdr (car cur)))
                                     (else
                                      (loop (cdr cur))))))
                           (let ((cached (__assqd? ',name (library-cache))))
                             (if cached
                                 cached
                                 ((lambda ()
                                    ,@(append include
                                              (strip body)
                                              (make-exporter name defines))))))))
                      wrap)
                     env)))))))

  (define (read-source path)
    (call-with-input-file path
      (lambda (port)
        (let loop ((res '())
                   (datum (get-datum port)))
          (if (eof-object? datum)
              (reverse res)
              (loop (cons datum res) (get-datum port)))))))

  (define (expand-file file)
	(let ((x (read-source file)))
	  (if (eq? (caar x) 'library)
		  (expand-library (car x))
		  (expand-top x))))

  (library-install! libraries-root
					'($builtin)
					(cons '()
						  (map (lambda (b)
								 (cons* (car b)
										(core-binding (cdr b))
										(make-label)))
							   `((quote . ,exp-quote)
								 (lambda . ,exp-lambda)
								 (define . ,exp-define)
								 (if . ,exp-if)
								 (set! . ,exp-set!)
								 (begin . ,exp-begin)
								 (syntax . ,exp-syntax)
								 (syntax-case . ,exp-syntax-case)
                                 (builtin-call . ,exp-builtin)
                                 ;; TODO add operator protections
								 ))))
  )
