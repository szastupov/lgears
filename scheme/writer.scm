;;;
;;; This code will be used instead of built-in display
;;;

(define (write-pair writer pair port)
  (put-char port #\()
  (let loop ((pair pair))
    (writer (car pair) port)
    (cond ((pair? (cdr pair))
           (put-char port #\space)
           (loop (cdr pair)))
          ((null? (cdr pair))
           #t)
          (else
            (put-string port " . ")
            (writer (cdr pair) port))))
  (put-char port #\)))

(define (write-vector writer vec port)
  (put-string port "#(")
  (let loop ((pos 0))
    (writer (vector-ref vec pos) port)
    (if (= (+ 1 pos) (vector-length vec))
      (put-char port #\))
      (begin
        (put-char port #\space)
        (loop (+ 1 pos))))))

(define (quote? obj)
  (and (pair? obj)
       (memq (car obj) '(quote quasiquote unquote unquote-splicing))))

(define (write-quote writer obj port)
  (put-string port
            (case (car obj)
              ((quote) "'")
              ((quasiquote) "`")
              ((unquote) ",")
              ((unquote-splicing) ",@")))
  (writer (cadr obj) port))

(define (write-common writer obj port)
  (cond ((quote? obj)
         (write-quote writer obj port))
        ((pair? obj)
         (write-pair writer obj port))
        ((null? obj)
         (put-string port "()"))
        ((vector? obj)
         (write-vector writer obj port))
        ((symbol? obj)
         (put-string port (symbol->string obj)))
        ((boolean? obj)
         (put-char port #\#)
         (put-char port (if obj #\t #\f)))
        ((number? obj)
         (put-string port (number->string obj)))
        (else
          (error 'write-common "unknown type" obj))))

(define (my-write obj port)
  (cond ((string? obj)
         (put-char port #\")
         (put-string port obj)
         (put-char port #\"))
        ((char? obj)
         (put-string port "#\\")
         (put-char port obj))
        (else (write-common my-write obj port))))

(define (my-display obj port)
  (cond ((string? obj)
         (put-string port obj))
        ((char? obj)
         (put-char port obj))
        (else (write-common my-display obj port))))

(my-display '(a . (b . (c . (d . (e . ()))))) (current-output-port))
