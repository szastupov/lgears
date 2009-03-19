(library (cpacked)
  (export define-cpacked)
  (import (rnrs))

  (define-syntax define-cpacked
    (lambda (stx)
      (define (s+ . args)
        (string->symbol
          (apply string-append args)))

      (define (type->bvproc type proc)
        (string->symbol
          (string-append "bytevector-" (symbol->string type) "-" proc)))

      (define (type->size type)
        (let ((str (symbol->string type)))
          (/ (string->number
               (substring str 1 (string-length str)))
             8)))

      (define (parse-type field)
        (let ((type (car field)))
          (case type
            ((u8 s8)
             (list (type->bvproc type "ref")
                   (type->bvproc type "set!")
                   (type->size type)))
            ((u16 s16 u32 s32 u64 s64)
             (list (type->bvproc type "native-ref")
                   (type->bvproc type "native-set!")
                   (type->size type)))
            (else (error 'parse-type "Wrong type description" type)))))

      (define (calc-offsets types)
        (let loop ((offsets '(0))
                   (type types))
          (if (null? type)
            (values (reverse (cdr offsets)) (car offsets))
            (loop (cons (+ (caddar type)
                           (car offsets))
                        offsets)
                  (cdr type)))))

      (syntax-case stx ()
        ((_ name . fields)
         (let* ((struct-name (symbol->string
                               (syntax->datum #'name)))
                (types (map parse-type (syntax->datum #'fields)))
                (names (map (lambda (n)
                              (symbol->string (cadr n))) (syntax->datum #'fields))))
           (let-values (((offsets size) (calc-offsets types)))
             #`(begin
                 (define (#,(s+ struct-name "-size"))
                   #,size)
                 #,@(map (lambda (name type offset)
                           #`(define (#,(s+ struct-name "-" name) st)
                               (#,(car type) st #,offset)))
                         names types offsets)
                 #,@(map (lambda (name type offset)
                           #`(define (#,(s+ struct-name "-" name "-set!") st val)
                               (#,(cadr type) st #,offset val)))
                         names types offsets)

                 (define setters
                   (vector
                     #,@(map (lambda (name)
                               #`(lambda (vec val)
                                   (#,(s+ struct-name "-" name "-set!") vec val)))
                             names)))

                 (define (#,(s+ "make-" struct-name) . args)
                   (let ((bv (make-bytevector #,size)))
                     (fold-left (lambda (idx arg)
                                  ((vector-ref setters idx) bv arg)
                                  (+ idx 1))
                                0 args)
                     bv)))))))))
  )
