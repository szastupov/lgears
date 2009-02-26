(import (rnrs)
        (cps)
        (assembly))

(define (set-func-args! ntbl args)
  (let loop ((idx 0) (lst args))
    (cond
      ((null? lst)
       idx)
      ((symbol? lst)
       (hashtable-set! ntbl lst idx)
       (+ idx 1))
      ((pair? lst)
       (hashtable-set! ntbl (car lst) idx)
       (loop (+ idx 1) (cdr lst)))
      (else
        (error 'make-env "unexpected" lst)))))

;; Environment of current-compiling function
(define-record-type env
  (fields parent
    tbl (mutable size)
    argc depth
    (mutable onheap)
    (mutable bindings)
    (mutable bindmap))
  (protocol
    (lambda (new)
      (lambda (prev args)
        (let* ((ntbl (make-eq-hashtable))
               (nargc (set-func-args! ntbl args))
               (ndepth (if (null? prev) 0
                         (+ 1(env-depth prev)))))
          (new prev ntbl nargc nargc ndepth #f '() '()))))))

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

(define (env-idx env name)
  (let ((res (hashtable-ref (env-tbl env) name #f)))
    (if res
      res
      (error 'env-idx "not found" name))))

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
        (code-store (make-store)))

    (define (compile-body env body)
      (define (code? x)
        (not (eq? (car x) 'extend)))
      (let ((extend (assq 'extend body))
            (code (filter code? body)))
        (if extend
          (for-each (lambda (def)
                      (env-define env def))
                    (cadr extend)))
        (map-append (lambda (expr)
                      (compile env expr))
                    code)))

    (define (compile-func parent args body)
      (let* ((env (make-env parent args))
             (compiled (compile-body env body))
             (cmd (if (null? (env-bindings env))
                    'LOAD_FUNC
                    'LOAD_CLOSURE)))
        `((,cmd
            ,(store-push! code-store
                          (make-func compiled env))
            1))))

    (define (compile-if env node)
      (let ((pred (compile env (car node)))
            (then-clause (compile env (cadr node)))
            (else-clause (if (null? (cddr node))
                           '()
                           (compile env (caddr node)))))
        `(,@pred
           (JUMP_IF_FALSE ,(+ (length then-clause) 1) 0)
           ,@then-clause
           ,@(if (null? else-clause)
               '()
               `((JUMP_FORWARD ,(length else-clause) 0)
                 ,@else-clause)))))

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
                 `((SET_LOCAL ,(cdr slot), -1))
                 `((SET_BIND ,(cdr slot) -1)))))))

    (define (compile env node)
      (cond ((pair? node)
             (case (car node)
               ((lambda)
                (compile-func env (cadr node) (cddr node)))
               ((if)
                (compile-if env (cdr node)))
               ((quote)
                `((LOAD_SYM ,(sym-table-insert symbols (cadr node)) 1)))
               ((set!)
                (compile-assigment env (cdr node)))
               (else
                 (compile-call env node))))
            ((number? node)
             `((LOAD_FIXNUM ,node 1)))
            ((string? node)
             (cons 'STRING node))
            ((boolean? node)
             `((LOAD_BOOL ,(if node 1 0) 1)))
            (else
              (let ((res (env-lookup env node)))
                (if res
                  (if (eq? (car res) 'LOCAL)
                    `((LOAD_LOCAL ,(cdr res) 1))
                    `((LOAD_BIND ,(cdr res) 1)))
                  `((LOAD_IMPORT ,(sym-table-insert undefs node) 1)))))))

    (let ((entry-point (compile-func '() '() root)))
      (make-ilr (symtable->list undefs)
                (symtable->list symbols)
                (reverse (store-head code-store))
                (cadar entry-point)))))

(let ((res (start-compile
             (cps-convert '( 
                            #|
                            (define lst (cons 'a (cons 'b 'c)))
                            (define (cadr x)
                              (car (cdr x)))
                            (define (cddr x)
                              (cdr (cdr x)))
                            (display (car lst))
                            (display (cadr lst))
                            (display (cddr lst))
                            |#

                            #|
                            (display 'start)
                            (display (call/cc
                                       (lambda (c)
                                         (c 'ok)
                                         (display 'failed))))
                            (display 'end)
                            |#

                            (display (* 10 2))

                            #|
                            (define (not x)
                              (if x #f #t))
                            |#

                            #|
                            (define (foo n)
                              (lambda (x)
                                (cons x n)))
                            (define fun (foo 'bar))
                            (display (cdr (fun 'zoo)))
                            |#
                            )))))
  (print-ilr res)
  (display "\nAssembly output:\n")
  (let ((port (open-file-output-port "/tmp/assembly" (file-options no-fail))))
    (assemble res port)
    (close-output-port port))
  )
