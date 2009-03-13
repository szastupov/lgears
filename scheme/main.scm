(import (rnrs)
        (compiler)
        (format))

(when (< (length (command-line)) 2)
  (format #t "Usage: ~a file.scm" (car (command-line)))
  (exit 1))

(compile-file (cadr (command-line)) "/tmp/assembly")
