(import (coreforms)
        ($builtin))

;(let f ((a 1)
        ;(b 2))
  ;(cond ((null? a) b)
        ;((foo? b) => (lambda (b) (+ b a)))
        ;(else 'fuck)))

;(unless foo
  ;1 2 3 4  5)

;(define (foo z . n)
  ;(display (+ z n)))

;(lambda (foo bar)
  ;(if foo bar))

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
