(include "base.scm")
#|
(define (range from to)
  (let loop ((step to)
             (res '()))
    (if (< step from)
      res
      (loop (- step 1) (cons step res)))))
(display (range 1 100))

(define (even? x)
  (= (mod x 2) 0))
(define (problem-2 lim)
  (define (loop n p r sum)
    (if (> r lim)
      sum
      (loop (+ 1 n) r (+ r p)
            (if (even? r)
              (+ sum r)
              sum))))
  (loop 2 1 1 0))

(display (problem-2 120))
|#

(define ttt 12)
(display `(1 2 3 ,ttt))
