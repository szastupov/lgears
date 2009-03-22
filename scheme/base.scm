;;
;; This library will be separated when modules will be ready
;;

(define (not x)
  (if x #f #t))

(define (zero? x)
  (= x 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (odd? x)
  (not (= (mod x 2) 0)))

(define (even? x)
  (= (mod x 2) 0))

(define (reverse lst)
  (let loop ((cur lst)
             (res '()))
    (if (null? cur)
      res
      (loop (cdr cur)
            (cons (car cur) res)))))

(define (for-each-1 proc lst)
  (if (null? lst)
    (void)
    (begin
      (proc (car lst))
      (for-each-1 proc (cdr lst)))))

(define for-each for-each-1)

(define (map-1 proc lst)
  (if (null? lst)
    '()
    (begin
      (cons (proc (car lst))
            (map-1 proc (cdr lst))))))

(define map map-1)

(define (fold-left-1 proc res lst)
  (if (null? lst)
    res
    (fold-left-1 proc (proc res (car lst)) (cdr lst))))

(define fold-left fold-left-1)

(define (vector-for-each-1 proc vec)
  (let loop ((n 0))
    (if (= n (vector-length vec))
      (void)
      (begin
        (proc (vector-ref vec n))
        (loop (+ 1 n))))))

(define vector-for-each vector-for-each-1)

(define (vector-map-1 proc vec)
  (let ((res (make-vector (vector-length vec))))
    (let loop ((n 0))
      (if (= n (vector-length vec))
        res
        (begin
          (vector-set! res n (proc (vector-ref vec n)))
          (loop (+ 1 n)))))))

(define vector-map vector-map-1)

(define (newline)
  (display "\n"))


;FIXME: check length and rewrote with let when it will be avaliable
(define (char=? . args)
  (let loop ((n (char->integer (car args)))
             (rest (cdr args)))
    (if (null? rest)
      #t
      (let ((c (char->integer (car rest))))
        (if (= c n)
          (loop c (cdr rest))
          #f)))))
