(define (range from to)
  (define (loop step res)
    (if (< step from)
      res
        (loop (- step 1) (cons step res))))
  (loop to '()))
(display (range 1 100))
