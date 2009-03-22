(include "base.scm")
#|
(define (range from to)
  (let loop ((step to)
             (res '()))
    (if (< step from)
      res
      (loop (- step 1) (cons step res)))))
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

(define (test-unary-pred pred arg expect)
  (test pred eq? (pred arg) expect))

(test-unary-pred boolean? #t #t)
(test-unary-pred boolean? #f #t)
(test-unary-pred boolean? 'foo #f)
(test-unary-pred procedure? 'foo #f)
(test-unary-pred procedure? test-unary-pred #t)
(test-unary-pred procedure? display #t)
(test-unary-pred pair? '(1 2) #t)
(test-unary-pred pair? '(1 . 2) #t)
(test-unary-pred pair? 'foo #f)
(test-unary-pred symbol? 'foo #t)
(test-unary-pred symbol? "foo" #f)
(test-unary-pred string? "foo" #t)
(test-unary-pred string? 'foo #f)
(test-unary-pred vector? '(1 2 3) #f)
(test-unary-pred vector? (vector 1 2 3) #t)
(test-unary-pred char? #\f #t)
(test-unary-pred char? 1 #f)
(test-unary-pred null? '(1) #f)
(test-unary-pred null? '() #t)
(test-unary-pred number? 42 #t)
(test-unary-pred number? #\n #f)

(define (test-binary-pred pred arg0 arg1 expect)
  (test pred eq? (pred arg0 arg1) expect))

(test-binary-pred equal? '(1 2 3) '(1 2 3) #t)
(test-binary-pred equal? '(1 2 3) '(1 2 73) #f)
(test-binary-pred equal? '(1 2 . 3) '(1 2 . 3) #t)
(test-binary-pred equal? '(1 2 . 3) '(1 2 . 8) #f)
(test-binary-pred equal? "foo" "foo" #t)
(test-binary-pred equal? "foo" "bar" #f)
;(test-binary-pred equal? (vector 1 2 3) (vector 1 2 3) #t)
;(test-binary-pred equal? (vector 1 2 5) (vector 1 2 3) #f)

(test apply = (apply + '(1 2 3 4 5)) 15)

(test call/cc = (call/cc
                    (lambda (c)
                      (c 42)
                      1)) 42)

(test char->integer = (char->integer #\f) 102)
