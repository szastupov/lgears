(import ($builtin)
        (coreforms))
;        (exceptions)

#|
(define (range from to)
  (let loop ((cur to)
             (res '()))
    (if ($= cur from)
      (cons cur res)
      (loop ($- cur 1) (cons cur res)))))

(let loop ((n 0))
  (if ($= n 100)
    (void)
    (begin
      (display (range 1 150))
      (loop ($+ n 1)))))



(display
(with-exception-handler
 (lambda (obj)
   (display "Got: ") (display obj)
   (display "\n")
   40)
 (lambda ()
   (raise-continuable "foo"))))
(display "\n")

;(raise-continuable 'foo)
|#

(display 1)
(display 'sym)
(display "str")
(display "\n")
(display '(1 2 3 4 5 6))