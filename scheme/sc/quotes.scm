(library (sc quotes)
  (export trquote trquasiquote)
  (import (rnrs))

  (define (self-eval? x)
    (or (number? x)
        (string? x)
        (char? x)
        (boolean? x)))

  ; Common transquote routine
  (define (trquote-common qv pfunc)
    (define (apply-pfunc x)
      (cond ((pair? x)
             (pfunc x))
            ((symbol? x)
             `(quote ,x))
            (else x)))

    (cond ((list? qv)
           (cons 'list (map apply-pfunc qv)))
          ((null? qv)
           '())
          ((pair? qv)
           `(cons ,(apply-pfunc (car qv))
                  ,(trquote-common (cdr qv) pfunc)))
          (else (apply-pfunc qv))))

  ; Translate quotation to functions
  (define (trquote qv)
    (trquote-common qv trquote))

  ; Translate quoasiquotation to functions
  ; FIXME: implement splicing
  (define (trquasiquote qv)
    (trquote-common
      qv
      (lambda (head)
        (case (car head)
          ((unquote)
           (cadr head))
          ((unquote-splicing)
           (cadr head))
          (else
            (trquasiquote head))))))

  )
