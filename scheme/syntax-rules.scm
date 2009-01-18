(library (syntax-rules)
  (export syntax-rules-compile)
  (import (rnrs))

  (define (syntax-rules-expand synobj node)
	'())

  (define (syntax-rules-compile name node)
	(display name)
	(newline)
	(display node)
	(newline)
	(let ([obj '()]
		  [keywords (car node)])
	  (lambda (cnode)
		(syntax-rules-expand obj cnode))))
  )
