;;
;; This library will be separated when modules will be ready
;;

(define (not x)
  (if x #f #t))

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

(define (null? x)
  (eq? x '()))

(define (reverse lst)
  (define (iter cur res)
    (if (null? cur)
      res
      (iter (cdr cur)
            (cons (car cur) res))))
  (iter lst '()))
