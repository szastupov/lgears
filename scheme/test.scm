(import (coreforms)
        ($builtin))

(let f ((a 1)
        (b 2))
  (cond ((null? a) b)
        ((foo? b) => (lambda (b) (+ b a)))
        (else 'fuck)))

(unless foo
  1 2 3 4  5)

(define (foo z . n)
  (display (+ z n)))