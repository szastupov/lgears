(library (base)
  (export dummy)
  (import (coreforms)
          ($builtin))

  (define-syntax dummy
    (syntax-rules ()
      ((_) foo)))

;; Arithmetic and numbers
  (define (+ . args)
    (fold-left $+ 0 args))

  (define (- init . args)
    (if (null? args)
        ($- 0 init)
        (fold-left $- init args)))

  (define (* . args)
    (fold-left $* 1 args))

  (define (/ init . args)
    (if (null? args)
        ($/ 1 init)
        (fold-left $/ init args)))

  (define (= init . args)
    (let loop ((arg args))
      (cond ((null? arg)
             #t)
            (($= init (car arg))
             (loop (cdr arg)))
            (else #f))))

  (define (cmp op init args)
    (let loop ((a init)
               (arg args))
      (cond ((null? arg)
             #t)
            ((op a (car arg))
             (loop (car arg) (cdr arg)))
            (else #f))))

  (define (< init . args)
    (cmp $< init args))

  (define (> init . args)
    (cmp $> init args))

  (define (find-m op init args)
    (fold-left (lambda (x y)
                 (if (op y x) y x))
               init args))

  (define (min init . args)
    (find-m < init args))

  (define (max init . args)
    (find-m > init args))

  (define (abs x)
    (if (> x 0)
        x
        (- x)))

  (define (zero? x)
    (= x 0))

  (define (positive? x)
    (> x 0))

  (define (negative? x)
    (< x 0))

  (define (odd? x)
    (not (= (mod x 2) 0)))

  (define (even? x)
    (= (mod x 2) 0))

;; Predicates

  (define (not x)
    (if x #f #t))

  ;; In context of lgears, eqv? and eq? is same
  (define eqv? eq?)

  (define (compare-pairs a b)
    (let loop ((a a)
               (b b))
      (cond ((and (null? a)
                  (null? b))
             #t)
            ((and (pair? a) (pair? b)
                  (eqv? (car a) (car b)))
             (loop (cdr a) (cdr b)))
            (else
             (eqv? a b)))))

  (define (compare-vectors a b)
    (if (= (vector-length a)
           (vector-length b))
        (let ((len (vector-length a)))
          (let loop ((n 0))
            (cond ((= n len)
                   #t)
                  ((eqv? (vector-ref a n)
                         (vector-ref b n))
                   (loop (+ 1 n)))
                  (else #f))))
        #f))

  (define (equal? a b)
    (cond ((and (string? a)
                (string? b))
           (string=? a b))
          ((and (pair? a)
                (pair? b))
           (compare-pairs a b))
          ((and (vector? a)
                (vector? b))
           (compare-vectors a b))
          (else
           (eqv? a b))))

;; List utilites

  (define (list? lst)
    (let loop ((cur lst))
      (cond ((null? cur) #t)
            ((pair? cur) (loop (cdr cur)))
            (else #f))))

  ;; Will be rewrited with exceptions and format
  (define (error what msg . irritians)
    (display "Error in ") (display what) (display ": ")
    (display msg) (display "\n")
    (for-each (lambda (i)
                (display "\t")
                (display i)
                (newline))
              irritians)
    (__exit 1))

  (define (length lst)
    (fold-left (lambda (x y) (+ 1 x)) 0 lst))

  (define (for-each proc lst1 . lst2)
    (define (for-each-1 lst)
      (if (null? lst)
          (void)
          (begin
            (proc (car lst))
            (for-each-1 (cdr lst)))))

    (define (for-each-n lst)
      (if (null? (car lst))
          (void)
          (begin
            (apply proc (map car lst))
            (for-each-n (map cdr lst)))))

    ;; TODO chech that length of lists is same
    (if (null? lst2)
        (for-each-1 lst1)
        (for-each-n (cons lst1 lst2))))

  (define (map proc lst1 . lst2)
    (define (map-1 lst)
      (if (null? lst)
          '()
          (begin
            (cons (proc (car lst))
                  (map-1 (cdr lst))))))

    (define (map-n lst)
      (if (null? (car lst))
          '()
          (cons (apply proc (map car lst))
                (map-n (map cdr lst)))))

    (if (null? lst2)
        (map-1 lst1)
        (map-n (cons lst1 lst2))))

  (define (fold-left proc init lst1 . lst2)
    (define (fold-left-1 res lst)
      (if (null? lst)
          res
          (fold-left-1 (proc res (car lst)) (cdr lst))))

    (define (fold-left-n res lst)
      (if (null? (car lst))
          res
          (fold-left-n (apply proc (cons res (map car lst)))
                       (map cdr lst))))

    (if (null? lst2)
        (fold-left-1 init lst1)
        (fold-left-n init (cons lst1 lst2))))

  (define (fold-right proc init lst1 . lst2)
    (define (fold-right-1 lst)
      (if (null? lst)
          init
          (proc (car lst) (fold-right-1 (cdr lst)))))

    (define (fold-right-n lst)
      (if (null? (car lst))
          init
          (apply proc (append (map car lst) (list (fold-right-n (map cdr lst)))))))

    (if (null? lst2)
        (fold-right-1 lst1)
        (fold-right-n (cons lst1 lst2))))


  (define (reverse lst)
    (fold-left (lambda (x y)
                 (cons y x))
               '() lst))

  (define (append2 b a)
    (fold-right cons b a))

  (define (append . args)
    (if (null? args)
        '()
        (let ((rev (reverse args)))
          (fold-left append2 (car rev) (cdr rev)))))

  (define (make-list count . args)
    ($make-list count (if (null? args)
                          #f
                          (car args))))

  (define (cons* . args)
    (let loop ((args args))
      (if (null? (cdr args))
          (car args)
          (cons (car args)
                (loop (cdr args))))))

  (define (find proc lst)
    (cond ((null? lst) #f)
          ((proc (car lst)) (car lst))
          (else (find proc (cdr lst)))))

  (define (memp proc lst)
    (cond ((null? lst) #f)
          ((proc (car lst)) lst)
          (else (memp proc (cdr lst)))))

  (define (mempred pred? obj lst)
    (memp (lambda (x)
            (pred? x obj))
          lst))

  (define (member obj lst)
    (mempred equal? obj lst))

  (define (memv obj lst)
    (mempred eqv? obj lst))

  (define (memq obj lst)
    (mempred eq? obj lst))

;; Vector utilites

  (define (vec-for-each-1 ref len proc vec)
    (let loop ((n 0))
      (if (= n (len vec))
          (void)
          (begin
            (proc (ref vec n))
            (loop (+ 1 n))))))

  (define (vector-for-each-1 proc vec)
    (vec-for-each-1 vector-ref vector-length proc vec))

  (define vector-for-each vector-for-each-1)

  (define (vector-map-1 proc vec)
    (let ((res (make-vector (vector-length vec))))
      (let loop ((n 0))
        (if (= n (vector-length vec))
            res
            (begin
              (vector-set! res n (proc (vector-ref vec n)))
              (loop (+ 1 n)))))))

  (define vector-map vector-map-1)

  ;; Rewrite it with case-lambda
  (define (make-vector size . args)
    ($make-vector size (if (null? args)
                           (void)
                           (car args))))

;; String and character utilites

  (define (string->list str)
    (let loop ((pos (- (string-length str) 1))
               (res '()))
      (if (= pos 0)
          (cons (string-ref str pos) res)
          (loop (- pos 1)
                (cons (string-ref str pos) res)))))

  (define (string-for-each-1 proc vec)
    (vec-for-each-1 string-ref string-length proc vec))

  (define string-for-each string-for-each-1)

  (define (string-append . args)
    (fold-left string-concat "" args))

  (define (char-op op args)
    (apply op (map char->integer args)))

  (define (char=? . args)
    (char-op = args))

  (define (char<? . args)
    (char-op < args))

  (define (char>? . args)
    (char-op > args))


  (define (test msg cmp arg expect)
    (display "Testing ") (display msg)
    (if (cmp arg expect)
        (display " ok\n")
        (begin
          (display " fail, expected ") (display expect) (display ", got ") (display arg) (display "\n"))))

  (define call-with-current-continuation call/cc)

  (define (newline)
    (display "\n"))

  (display "Base library loaded\n")
  )