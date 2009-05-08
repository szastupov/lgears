(library (core_arithmetic)
  (export + - * / = < > min max abs zero? positive? odd? even?)
  (import ($builtin)
          (coreforms)
          (core_sequence))

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

  )