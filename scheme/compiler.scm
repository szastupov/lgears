(library (compiler)
  (export compile-file expand-file cps-convert dump-ilr)
  (import (rnrs)
          (sc compiler)
          (sc expand)
          (sc cps)
          (only (core) pretty-print)) ; This works only for ypsilon

  (define (expand-file file)
    (expand '() '() (read-source file)))
  )
