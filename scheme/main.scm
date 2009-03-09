(import (rnrs)
        (compiler))

(define (testexpr)
(compile-expr '(
                #|
                (display 'start)
                (display (call/cc
                           (lambda (c)
                             (c 'ok)
                             (display 'failed))))
                (display 'end)
                |#

                #|
                (define (f-aux n a)
                  (if (= 0 n)
                    a
                    (f-aux (- n 1) (* n a))))
                (display (f-aux 100 1))
                (display "ok")
                |#

                ;#|
                (define (range from to)
                  (define (loop step res)
                    (if (< step from)
                      res
                      (loop (- step 1) (cons step res))))
                  (loop to '()))
                (display (range 1 100))
                ;|#

                #|
                (define (foo n)
                  (lambda (x)
                    (cons x n)))
                (display (car ((foo 'bar) 'zoo)))
                |#
                )
              "/tmp/assembly"))

(define (testfile)
  (compile-file "/home/redchrom/psyntax.pp" "/tmp/assembly"))

(testexpr)
;(testfile)
