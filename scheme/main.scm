#!/usr/bin/env ypsilon
(add-library-path ".")

(import (rnrs)
        (compiler)
        (format))

(define (full-compile file)
  (let ((assm (string-append "compiled/" file ".o")))
    (if (not (file-exists? "compiled"))
      (create-directory "compiled"))
    (compile-file file assm)))

(let* ((argv (command-line))
       (argc (length argv)))
  (cond ((< argc 2)
         (format #t "Usage: ~a [-E -C -A] file.scm\n" (car argv))
         (exit 1))
        ((= argc 3)
         (let* ((flag (cadr argv))
                (file (caddr argv)))
           (cond ((equal? flag "-E")
                  (format #t ";;; Expanded from ~a\n" file)
                  (pretty-print (car (expand-file file))))

                 ((equal? flag "-C")
                  (format #t ";;; CPS-converted from ~a\n" file)
                  (pretty-print (cps-convert (expand-file file))))

                 ((equal? flag "-A")
                  (dump-ilr file))

                 (else
                   (error 'main "unknown option" (cadr argv))))))
        (else
          (full-compile (cadr argv)))))
(newline)