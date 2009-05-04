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
(display (length '(1 2 3 4 5)))
