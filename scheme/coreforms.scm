(define-syntax and
  (lambda (x)
    (let ((body (reverse (cdr x))))
      (fold-left (lambda (prev cur)
                   `(if ,cur
                      ,prev
                      #f))
                 (car body) (cdr body)))))

(define-syntax let*
  (lambda (stx)
    (let loop ((args (cadr stx)))
      (if (null? args)
        (cons 'begin (cddr stx))
        (let ((cur (car args)))
          `((lambda (,(car cur))
              ,(loop (cdr args)))
            ,(cadr cur)))))))


(define-syntax let
  (lambda (stx)
    (define (expand-let node)
      (define (get-vals node)
        (map cadr node))

      (define (impl node)
        (let ((names (map car (car node)))
              (exprs (map cadr (car node))))
          `(lambda ,names
             (begin ,@(cdr node)))))

      (if (symbol? (car node))
        `(let ((,(car node) 'unspec))
           (set! ,(car node) ,(impl (cdr node)))
           (,(car node) ,@(get-vals (cadr node))))
        `(,(impl node)
           ,@(get-vals (car node)))))
    (expand-let (cdr stx))))

(define-syntax letrec
  (lambda (stx)
    (let ((names (map car (cadr stx))))
      `((lambda ,names
          ,@(map (lambda (name val)
                   `(set! ,name ,val))
                 names (map cadr (cadr stx)))
          ,@(cddr stx))
        ,@(map (lambda (x) ''unspec) names)))))

(define-syntax cond
  (lambda (stx)
    (define (expand-cond node)
      (define (expand-cond-var prev var)
        `(if ,(car var)
           (begin ,@(cdr var))
           ,prev))
      (let ((body (reverse node)))
        (if (eq? (caar body) 'else)
          (fold-left expand-cond-var (cadar body) (cdr body))
          (fold-left expand-cond-var '(void) body))))
    (expand-cond (cdr stx))))

(define-syntax quasiquote
  (lambda (stx)
    (trquasiquote (cadr stx))))
