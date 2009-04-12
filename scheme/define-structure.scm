(library (define-structure)
  (export define-structure)
  (import (rnrs))

  (define-syntax define-structure
    (lambda (x)
      (define construct-name
        (lambda (template-identifier . args)
          (datum->syntax
            template-identifier
            (string->symbol
              (apply string-append
                     (map (lambda (x)
                            (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                          args))))))
      (syntax-case x ()
        ((_ name id1 ...)
         (for-all identifier? (syntax (name id1 ...)))
         (with-syntax
           ((constructor (construct-name (syntax name) "make-" (syntax name)))
            (predicate (construct-name (syntax name) (syntax name) "?"))
            ((access ...)
             (map (lambda (x) (construct-name x (syntax name) "-" x))
                  (syntax (id1 ...))))
            ((assign ...)
             (map (lambda (x)
                    (construct-name x "set-" (syntax name) "-" x "!"))
                  (syntax (id1 ...))))
            (structure-length
              (+ (length (syntax (id1 ...))) 1))
            ((index ...)
             (let f ((i 1) (ids (syntax (id1 ...))))
               (if (null? ids)
                 '()
                 (cons i (f (+ i 1) (cdr ids)))))))
           (syntax (begin
                     (define constructor
                       (lambda (id1 ...)
                         (vector 'name id1 ... )))
                     (define predicate
                       (lambda (x)
                         (and (vector? x)
                              (= (vector-length x) structure-length)
                              (eq? (vector-ref x 0) 'name))))
                     (define access
                       (lambda (x)
                         (assert (predicate x))
                         (vector-ref x index)))
                     ...
                     (define assign
                       (lambda (x update)
                         (assert (predicate x))
                         (vector-set! x index update)))
                     ...)))))))
  )
