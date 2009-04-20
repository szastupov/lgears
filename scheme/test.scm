(import (coreforms)
        ($builtin))

(let f ((a 1)
        (b 2))
  (cond ((null? a) b)
        ((foo? b) => (lambda (b) (+ b a)))
        (else 'fuck)))