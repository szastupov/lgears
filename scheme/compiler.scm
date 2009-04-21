(library (compiler)
  (export compile-file expand-file cps-convert dump-ilr)
  (import (rnrs)
          (sc expand)
          (sc cps)
          (sc compiler)
          (sc assembly)
          (only (core) pretty-print)) ; This works only for ypsilon

  (define (compile-ilr-file in)
    (start-compile
      (cps-convert
       (expand-file in))))

  (define (compile-file in out)
    (assemble (compile-ilr-file in)
              out))

  (define (dump-ilr path)
    (print-ilr (compile-ilr-file path)))

  )
