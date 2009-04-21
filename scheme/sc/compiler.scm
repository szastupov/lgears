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

(library (sc compiler)
  (export start-compile)
  (import (rnrs)
          (reader)
          (format)
          (only (core) pretty-print) ; This works only for ypsilon
          (sc assembly))

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

  (define (start-compile root)
    (let ((undefs (make-sym-table))
          (symbols (make-sym-table))
          (code-store (make-store))
          (string-store (make-store)))

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
          `((LOAD_CLOSURE ,idx 1))))
          ;(if (null? (env-bindings env))
            ;`((LOAD_FUNC ,idx 1))
            ;`((LOAD_CLOSURE ,idx 1)))))

      (define (compile-if env node)
        (let ((pred (compile env (car node)))
              (then-clause (compile env (cadr node)))
              (else-clause (compile env (caddr node))))
          `(,@pred
             (JUMP_IF_FALSE ,(+ (length then-clause) 1) 1)
             ,@then-clause
             (JUMP_FORWARD ,(length else-clause) 0)
             ,@else-clause)))

      (define (compile-call env node)
        (let* ((func (compile env (car node)))
               (argc (length (cdr node)))
               (args (map-append (lambda (x)
                                   (compile env x))
                                 (cdr node))))
          `(,@args ,@func
                   (FUNC_CALL ,argc ,(- argc)))))

      (define (compile-assigment env node)
        (let* ((name (car node))
               (slot (env-lookup env name)))
          (if (not slot)
            (error 'compile-assigment "undefined variable" name)
            `(,@(compile env (cadr node))
               ,@(if (eq? (car slot) 'LOCAL)
                   `((SET_LOCAL ,(cdr slot), -1 ,name))
                   `((SET_BIND ,(cdr slot) -1 ,name)))))))

      (define (compile env node)
        (cond ((pair? node)
               (case (car node)
                 ((lambda)
                  (compile-func env (cadr node) (cddr node)))
                 ((if)
                  (compile-if env (cdr node)))
                 ((quote)
                  (if (null? (cadr node))
                    `((LOAD_CONST 0 1))
                    `((LOAD_SYM ,(sym-table-insert symbols (cadr node)) 1 ,(cadr node)))))
                 ((set!)
                  (compile-assigment env (cdr node)))
                 (else
                   (compile-call env node))))
              ((char? node)
               `((PUSH_CHAR ,(char->integer node) 1)))
              ((number? node)
               `((PUSH_FIXNUM ,node 1)))
              ((string? node)
               `((LOAD_STRING ,(store-push! string-store node) 1 ,node)))
              ((boolean? node)
               `((PUSH_BOOL ,(if node 1 0) 1)))
              (else
                (let ((res (env-lookup env node)))
                  (if res
                    (if (eq? (car res) 'LOCAL)
                      `((LOAD_LOCAL ,(cdr res) 1 ,node))
                      `((LOAD_BIND ,(cdr res) 1 ,node)))
                    `((LOAD_IMPORT ,(sym-table-insert undefs node) 1 ,node)))))))

      (let ((entry-point (compile-func '() '() root)))
        (make-ilr (symtable->list undefs)
                  (symtable->list symbols)
                  (reverse (store-head code-store))
                  (reverse (store-head string-store))
                  (cadar entry-point)))))
  )
