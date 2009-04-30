#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov
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

(library (sc compiler)
  (export start-compile)
  (import (rnrs)
		  (reader)
		  (format)
		  (only (core) pretty-print) ; This works only for ypsilon
		  (sc fasl))

  (define (set-func-args! ntbl args)
	(let loop ((idx 0) (lst args))
	  (cond
		((null? lst)
		 (cons #f idx))
		((symbol? lst)
		 (hashtable-set! ntbl lst idx)
		 (cons #t idx))
		((pair? lst)
		 (hashtable-set! ntbl (car lst) idx)
		 (loop (+ idx 1) (cdr lst)))
		(else
		  (error 'make-env "unexpected" lst)))))

  ;; Environment of current-compiling function
  (define-record-type env
	(fields parent
	  tbl (mutable size)
	  argc swallow depth
	  (mutable onheap)
	  (mutable bindings)
	  (mutable bindmap))
	(protocol
	  (lambda (new)
		(lambda (prev args)
		  (let* ((ntbl (make-eq-hashtable))
				 (nargc (set-func-args! ntbl args))
				 (ndepth (if (null? prev) 0
						   (+ 1(env-depth prev))))
				 (nsize (if (car nargc)
						  (+ (cdr nargc) 1)
						  (cdr nargc))))
			(new prev ntbl nsize (cdr nargc)
				 (car nargc) ndepth #f '() '()))))))

  (define (env-define env name)
	(let ((size (env-size env)))
	  (hashtable-set! (env-tbl env) name size)
	  (env-size-set! env (+ size 1))))

  (define (env-bind env up idx)
	(define (set-binding! midx)
	  (let* ((bindings (env-bindings env))
			 (nmidx (- midx 1))
			 (key (cons nmidx idx))
			 (found (member key bindings)))
		(if found
		  (- (length found) 1)
		  (let ((newbind (cons key bindings)))
			(env-bindings-set! env newbind)
			(- (length newbind) 1)))))

	(let* ((bindmap (env-bindmap env))
		   (inmap (memv up bindmap)))
	  (if inmap
		(set-binding! (length inmap))
		(let ((nb (cons up bindmap)))
		  (env-bindmap-set! env nb)
		  (set-binding! (length nb))))))

  (define (make-func code env)
	(make-i-func
	  code
	  (env-size env)
	  (env-argc env)
	  (env-swallow env)
	  (env-onheap env)
	  (env-depth env)
	  (reverse (env-bindings env))
	  (reverse (env-bindmap env))))

  (define (env-lookup env name)
	(let loop ((step 0)
			   (cur-env env))
	  (if (null? cur-env)
		#f
		(let ((res (hashtable-ref (env-tbl cur-env) name #f)))
		  (if res
			(if (zero? step)
			  `(LOCAL . ,res)
			  (begin
				(env-onheap-set! cur-env #t)
				`(BINDING . ,(env-bind env step res))))
			(loop (+ step 1) (env-parent cur-env)))))))

  (define-record-type sym-table
	(fields table (mutable count))
	(protocol
	  (lambda (new)
		(lambda () (new (make-eq-hashtable) 0)))))

  (define (sym-table-insert stbl sym)
	(let* ((tbl (sym-table-table stbl))
		   (res (hashtable-ref tbl sym #f)))
	  (if res
		res
		(begin
		  (set! res (sym-table-count stbl))
		  (hashtable-set! tbl sym res)
		  (sym-table-count-set! stbl (+ res 1))
		  res))))

  ;; Return list of keys sorted by value (index)
  ;;
  ;; As order of returned hash values is not specified, we have
  ;; to guaranty that everything is ok.
  (define (symtable->list stbl)
	(let-values (((keys vals) (hashtable-entries (sym-table-table stbl))))
	  (map (lambda (x)
			 (symbol->string (car x)))
		   (list-sort (lambda (x y) (< (cdr x) (cdr y)))
					  (map cons
						   (vector->list keys)
						   (vector->list vals))))))

  (define-record-type store
	(fields (mutable head) (mutable count))
	(protocol
	  (lambda (new)
		(lambda () (new '() 0)))))

  (define (store-push! store val)
	(let ((res (store-count store)))
	  (store-count-set! store (+ 1 res))
	  (store-head-set! store (cons val (store-head store)))
	  res))

  (define (map-append proc lst)
	(if (null? lst)
	  '()
	  (append (proc (car lst))
			  (map-append proc (cdr lst)))))

  (define (datum->string dt)
    (cond ((string? dt) dt)
          ((symbol? dt) (symbol->string dt))
          ((char? dt) (string dt))
          ((number? dt) (number->string dt))
          ((boolean? dt) (if dt "#t" "#f"))
          (else (error 'datum->string "unknown datum" dt))))

  (define (start-compile root)
	(let ((undefs (make-sym-table))
		  (code-store (make-store))
		  (consts '()))

	  (define (make-const datum)
		(cond ((assoc datum consts)
			   => cdr)
			  (else
			   (let ((idx (length consts)))
				 (set! consts (cons (cons datum idx)
									consts))
				 idx))))

	  (define (split-extend node)
		(if (and (pair? (car node))
			 (eq? (caar node) 'extend))
			 (values (cadar node)
					 (cdr node))
			 (values #f node)))

	  (define (compile-body env body)
		(define (code? x)
		  (not (eq? (car x) 'extend)))
		(let-values (((extend code) (split-extend body)))
		  (if extend
			(for-each (lambda (def)
						(env-define env def))
					  extend))
		  (map-append (lambda (expr)
						(compile env expr))
					  code)))

	  (define (compile-func parent args body)
		(let* ((env (make-env parent args))
			   (compiled (compile-body env body))
			   (idx (store-push! code-store
								 (make-func compiled env))))
		  `((LOAD_CLOSURE ,idx))))
		  ;(if (null? (env-bindings env))
			;`((LOAD_FUNC ,idx))
			;`((LOAD_CLOSURE ,idx)))))

	  (define (compile-if env node)
		(let ((pred (compile env (car node)))
			  (then-clause (compile env (cadr node)))
			  (else-clause (compile env (caddr node))))
		  `(,@pred
			 (JUMP_IF_FALSE ,(+ (length then-clause) 1))
			 ,@then-clause
			 (JUMP_FORWARD ,(length else-clause))
			 ,@else-clause)))

	  (define (compile-call env node)
		(let* ((func (compile env (car node)))
			   (argc (length (cdr node)))
			   (args (map-append (lambda (x)
								   (compile env x))
								 (cdr node))))
		  `(,@args ,@func
				   (FUNC_CALL ,argc))))

	  (define (compile-assigment env node)
		(let* ((name (car node))
			   (slot (env-lookup env name)))
		  (if (not slot)
			(error 'compile-assigment "undefined variable" name)
			`(,@(compile env (cadr node))
			   ,@(if (eq? (car slot) 'LOCAL)
				   `((SET_LOCAL ,(cdr slot)
                                ,(datum->string name)))
				   `((SET_BIND ,(cdr slot)
                               ,(datum->string name))))))))

	  (define (compile env node)
		(cond ((pair? node)
			   (case (car node)
				 ((lambda)
				  (compile-func env (cadr node) (cddr node)))
				 ((if)
				  (compile-if env (cdr node)))
				 ((quote)
				  (if (null? (cadr node))
					`((PUSH_NULL 0))
					`((LOAD_CONST ,(make-const (cadr node))
                                  ,(datum->string (cadr node))))))
				 ((set!)
				  (compile-assigment env (cdr node)))
				 (else
				   (compile-call env node))))
			  ((or (char? node)
				   (number? node)
				   (string? node))
			   `((LOAD_CONST ,(make-const node)
                             ,(datum->string node))))
			  ((boolean? node)
			   `((PUSH_BOOL ,(if node 1 0)
                            ,(datum->string node))))
			  (else
				(let ((res (env-lookup env node)))
				  (if res
					(if (eq? (car res) 'LOCAL)
					  `((LOAD_LOCAL ,(cdr res)
                                    ,(datum->string node)))
					  `((LOAD_BIND ,(cdr res)
                                   ,(datum->string node))))
					`((LOAD_IMPORT ,(sym-table-insert undefs node)
                                   ,(datum->string node))))))))

	  (define (dispatch-input)
		(case (car root)
		  ((top-level)
		   (values (cadr root)
				   (compile-func '() '() (cddr root))))
		  ((library)
		   (let ((hdr (cadr root)))
			 (values (cadr hdr)
					 (compile '() (cddr root)))))
		  (else (error 'dispatch-input "wtf?"))))

	  (let-values (((imports entry-point) (dispatch-input)))
		(make-ilr (symtable->list undefs)
				  (map car (reverse consts))
				  (reverse (store-head code-store))
				  (cadar entry-point)
				  imports))))
  )
