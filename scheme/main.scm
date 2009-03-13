#!/usr/bin/env ypsilon
(add-library-path ".")

(import (rnrs)
        (lgears compiler)
        (lgears format))

(when (< (length (command-line)) 2)
  (format #t "Usage: ~a file.scm\n" (car (command-line)))
  (exit 1))

(compile-file (cadr (command-line)) "/tmp/assembly")
