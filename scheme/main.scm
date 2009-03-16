#!/usr/bin/env ypsilon
(add-library-path ".")

(import (rnrs)
        (compiler)
        (format))

(when (< (length (command-line)) 2)
  (format #t "Usage: ~a file.scm\n" (car (command-line)))
  (exit 1))

(let* ((source (cadr (command-line)))
       (assm (string-append "compiled/" source ".o")))
  (if (not (file-exists? "compiled"))
    (create-directory "compiled"))
  (compile-file source assm))
