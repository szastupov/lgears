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

;; In context of lgears, eqv? and eq? are same
(define eqv? eq?)

(define (compare-pairs a b)
  (let loop ((a a)
             (b b))
    (cond ((and (null? a)
                (null? b))
           #t)
          ((and (pair? a) (pair? b)
                (eqv? (car a) (car b)))
           (loop (cdr a) (cdr b)))
          (else
            (eqv? a b)))))

(define (compare-vectors a b)
  (if (= (vector-length a)
         (vector-length b))
    (let ((len (vector-length a)))
      (let loop ((n 0))
        (cond ((= n len)
               #t)
              ((eqv? (vector-ref a n)
                     (vector-ref b n))
               (loop (+ 1 n)))
              (else #f))))
    #f))

(define (equal? a b)
  (cond ((and (string? a)
              (string? b))
         (string=? a b))
        ((and (pair? a)
              (pair? b))
         (compare-pairs a b))
        ((and (vector? a)
              (vector? b))
         (compare-vectors a b))
        (else
          (eqv? a b))))

(define (reverse lst)
  (let loop ((cur lst)
             (res '()))
    (if (null? cur)
      res
      (loop (cdr cur)
            (cons (car cur) res)))))

(define (for-each proc lst1 . lst2)
  (define (for-each-1 lst)
    (if (null? lst)
      (void)
      (begin
        (proc (car lst))
        (for-each-1 (cdr lst)))))

  (define (for-each-n lst)
    (if (null? (car lst))
      (void)
      (begin
        (apply proc (map car lst))
        (for-each-n (map cdr lst)))))

  ;; TODO chech that length of lists is same
  (if (null? lst2)
    (for-each-1 lst1)
    (for-each-n (cons lst1 lst2))))

(define (map proc lst1 . lst2)
  (define (map-1 lst)
    (if (null? lst)
      '()
      (begin
        (cons (proc (car lst))
              (map-1 (cdr lst))))))

  (define (map-n lst)
    (if (null? (car lst))
      '()
      (cons (apply proc (map car lst))
            (map-n (map cdr lst)))))

  (if (null? lst2)
    (map-1 lst1)
    (map-n (cons lst1 lst2))))

(define (fold-left-1 proc res lst)
  (if (null? lst)
    res
    (fold-left-1 proc (proc res (car lst)) (cdr lst))))

(define fold-left fold-left-1)

(define (vec-for-each-1 ref len proc vec)
  (let loop ((n 0))
    (if (= n (len vec))
      (void)
      (begin
        (proc (ref vec n))
        (loop (+ 1 n))))))

(define (vector-for-each-1 proc vec)
  (vec-for-each-1 vector-ref vector-length proc vec))

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

(define (string->list str)
  (let loop ((pos (- (string-length str) 1))
             (res '()))
    (if (= pos 0)
      (cons (string-ref str pos) res)
      (loop (- pos 1)
            (cons (string-ref str pos) res)))))

(define (string-for-each-1 proc vec)
  (vec-for-each-1 string-ref string-length proc vec))

(define string-for-each string-for-each-1)

(define (newline)
  (display "\n"))

;FIXME: check length
(define (char=? . args)
  (let loop ((n (char->integer (car args)))
             (rest (cdr args)))
    (if (null? rest)
      #t
      (let ((c (char->integer (car rest))))
        (if (= c n)
          (loop c (cdr rest))
          #f)))))

(define (test msg cmp arg expect)
  (display "Testing ") (display msg)
  (if (cmp arg expect)
    (display " ok\n")
    (begin
      ; UGLY!!!!
      (display " fail, expected ") (display expect) (display ", got ") (display arg) (display "\n"))))
