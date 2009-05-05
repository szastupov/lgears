(import ($builtin)
        (coreforms))

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
|#

(define (default-exception obj)
  (display "Unhandled excetion with: ")
  (display obj)
  (display "\n")
  (__exit))

(define exception-handler (list default-exception))

(define (with-exception-handler new thunk)
  (let ((parent exception-handler))
    (set! exception-handler (cons new parent))
    (let ((res (thunk)))
      (set! exception-handler parent)
      res)))

(define (raise x)
  (let ((handlers exception-handler))
    (set! exception-handler (cdr handlers))
    ((car handlers) x)
    (__exit)))

(define (raise-continuable x)
  (let ((handlers exception-handler))
    (set! exception-handler (cdr handlers))
    (let ((res ((car handlers) x)))
      (set! exception-handler handlers)
      res)))

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

(display exception-handler)