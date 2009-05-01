(library (config)
  (export get-cache-path)
  (import (rnrs base)
          (only (core) getenv))

  (define (get-cache-path)
    (cond ((getenv "LGEARS_CACHE")
           => (lambda (p) p))
          (else
           (string-append (getenv "HOME") "/.cache/lgears"))))

  )