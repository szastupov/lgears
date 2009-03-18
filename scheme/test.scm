#|
(include "base.scm")
(define (range from to)
 (define (loop step res)
   (if (< step from)
     res
     (loop (- step 1) (cons step res))))
 (loop to '()))
(display (range 1 100))

;(define (even? x)
  ;(= (mod x 2) 0))

;(define (problem-2 lim)
  ;(define (loop n p r sum)
    ;(if (> r lim)
      ;sum
      ;(loop (+ 1 n) r (+ r p)
            ;(if (even? r)
              ;(+ sum r)
              ;sum))))
  ;(loop 2 1 1 0))

;(display (problem-2 120))
|#

(define (test msg cmp arg expect)
  (display "Testing ") (display msg)
  (if (cmp arg expect)
    (display " ok\n")
    (begin
      ; UGLY!!!!
      (display " fail, expected ") (display expect) (display ", got ") (display arg) (display "\n"))))

(define (test-pred pred arg expect)
  (test pred eq? (pred arg) expect))

(test-pred boolean? #t #t)
(test-pred boolean? #f #t)
(test-pred boolean? 'foo #f)
(test-pred procedure? 'foo #f)
(test-pred procedure? test-pred #t)
(test-pred procedure? display #t)
(test-pred pair? '(1 2) #t)
(test-pred pair? '(1 . 2) #t)
(test-pred pair? 'foo #f)
(test-pred symbol? 'foo #t)
(test-pred symbol? "foo" #f)

(test apply = (apply + '(1 2 3 4 5)) 15)

(test call/cc = (call/cc
                    (lambda (c)
                      (c 42)
                      1)) 42)

