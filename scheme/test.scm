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

(define (with-exception-handler handler thunk)
  (let ((parent (exception-handler))
        (die? (call/cc
                    (lambda (return)
                      (exception-handler-set!
                       (cons (lambda (die? obj)
                               (handler obj)
                               (return die?))
                             (exception-handler)))
                      (thunk)))))
    (exception-handler-set! parent)
    (if die?
        (__exit))))

(define (raise-continuable x)
  ((car (exception-handler)) #f x))

(define (raise x)
  ((car (exception-handler)) #t x))

(with-exception-handler
 (lambda (obj)
   (display "Got: ") (display obj)
   (display "\n"))
 (lambda ()
   (raise-continuable '(1 2 3))))

(display (exception-handler))