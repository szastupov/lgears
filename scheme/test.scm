(import (coreforms)
        ($builtin))

(define (range from to)
  (let loop ((cur to)
             (res '()))
    (if ($= cur from)
      (cons cur res)
      (loop ($- cur 1) (cons cur res)))))

(let loop ((n 0))
  (if ($= n 5)
    (void)
    (begin
      (display (range 1 150))
      (loop ($+ n 1)))))

;(display 'ok)
;(display 10000)
;(display #\п)
;(display "bla")
