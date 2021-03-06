#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
 |
 | This program is free software; you can redistribute it and/or
 | modify it under the terms of the GNU Lesser General Public
 | License as published by the Free Software Foundation; either
 | version 3 of the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 | Lesser General Public License for more details.
 |
 | You should have received a copy of the GNU Lesser General
 | Public Licens along with this program, if not; see
 | <http://www.gnu.org/licenses>.
 |#
(library (core.forms)
  (export quote lambda define if set! begin syntax syntax-case builtin-call
          with-syntax syntax-rules or and let let* cond case unquote unquote-splicing
          when unless not cons car cdr eq? op-binary op-unary fixnum?
          null? list? pair? procedure? boolean? char? symbol? struct?
          string? bytevector? eof-object?)
  (import ($builtin))

  (define-syntax with-syntax
	(lambda (x)
	  (syntax-case x ()
		((_ () e1 e2 ...)
		 (syntax (begin e1 e2 ...)))
		((_ ((out in)) e1 e2 ...)
		 (syntax (syntax-case in () (out (begin e1 e2 ...)))))
		((_ ((out in) ...) e1 e2 ...)
		 (syntax (syntax-case (list in ...) ()
				   ((out ...) (begin e1 e2 ...))))))))

  (define-syntax syntax-rules
	(lambda (x)
	  (define (clause y)
		(syntax-case y ()
		  (((keyword . pattern) template)
		   (syntax ((dummy . pattern) (syntax template))))
		  (((keyword . pattern) fender template)
		   (syntax ((dummy . pattern) fender (syntax template))))
		  (_ (syntax-error x))))
	  (syntax-case x ()
		((_ (k ...) cl ...)
		 (with-syntax (((ccl ...) (map clause #'(cl ...))))
		   (syntax (lambda (x) (syntax-case x (k ...) ccl ...))))))))

  (define-syntax or
	(syntax-rules ()
	  ((_) #f)
	  ((_ a) a)
	  ((_ a b ...)
	   (let ((t a))
		 (if t t (or b ...))))))

  (define-syntax and
	(syntax-rules ()
	  ((_) #t)
	  ((_ a) a)
	  ((_ a b ...)
	   (if a (and b ...) #f))))

  (define-syntax let
    (syntax-rules ()
      ((_ ((vars vals) ...) e1 e2 ...)
       ((lambda (vars ...)
          e1 e2 ...)
        vals ...))
      ((_ loop ((vars vals) ...) e1 e2 ...)
       (let ()
         (define loop (lambda (vars ...)
                        e1 e2 ...))
         (loop vals ...)))))

  (define-syntax let*
    (syntax-rules ()
      ((_ () e1 e2 ...)
       (let () e1 e2 ...))
      ((_ ((var1 val1) head ...)
          body1 body2 ...)
       (let ((var1 val1))
         (let* (head ...)
           body1 body2 ...)))))

  (define-syntax cond
    (lambda (x)
      (syntax-case x ()
        ((_ m1 m2 ...)
         (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
           (if (null? clauses)
               (syntax-case clause (else =>)
                 ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                 ((e0) (syntax (let ((t e0)) (if t t))))
                 ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t)))))
                 ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...))))
                 (_ (syntax-error x)))
               (with-syntax ((rest (f (car clauses) (cdr clauses))))
                 (syntax-case clause (else =>)
                   ((e0) (syntax (let ((t e0)) (if t t rest))))
                   ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t) rest))))
                   ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                   (_ (syntax-error x))))))))))

  (define-syntax case
    (lambda (x)
      (syntax-case x ()
        ((_ e m1 m2 ...)
         (with-syntax
             ((body (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
                      (if (null? clauses)
                          (syntax-case clause (else)
                            ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                            (((k ...) e1 e2 ...)
                             (syntax (if (memv t '(k ...)) (begin e1 e2 ...))))
                            (_ (syntax-error x)))
                          (with-syntax ((rest (f (car clauses) (cdr clauses))))
                            (syntax-case clause (else)
                              (((k ...) e1 e2 ...)
                               (syntax (if (memv t '(k ...))
                                           (begin e1 e2 ...)
                                           rest)))
                              (_ (syntax-error x))))))))
           (syntax (let ((t e)) body)))))))

  (define-syntax unquote
    (lambda (x)
      (syntax-error x "misplaced")))

  (define-syntax unquote-splicing
    (lambda (x)
      (syntax-error x "misplaced")))

  (define-syntax when
	(syntax-rules ()
	  ((when test result1 result2 ...)
	   (if test
		   (begin result1 result2 ...)))))

  (define-syntax unless
	(syntax-rules ()
	  ((unless test result1 result2 ...)
	   (if (not test)
		   (begin result1 result2 ...)))))

  (define-syntax op-unary
    (lambda (x)
      (syntax-case x ()
        ((_ op)
         #'(lambda (y)
             (syntax-case y ()
               ((_ a) #'(op a))
               (id (identifier? #'id)
                   #'(lambda (a) (op a)))))))))

  (define-syntax op-binary
    (lambda (x)
      (syntax-case x ()
        ((_ op)
         #'(lambda (y)
             (syntax-case y ()
               ((_ a b) #'(op a b))
               (id (identifier? #'id)
                   #'(lambda (a b) (op a b)))))))))

  (define-syntax car
    (op-unary $car))

  (define-syntax cdr
    (op-unary $cdr))

  (define-syntax not
    (op-unary $!))

  (define-syntax cons
    (op-binary $cons))

  (define-syntax eq?
    (op-binary $eq?))

  (define-syntax type-test
    (lambda (x)
      (syntax-case x ()
        ((_ tname)
         #'(lambda (y)
             (syntax-case y ()
               ((_ a) #'($type-test tname a))
               (id (identifier? #'id)
                   #'(lambda (a) ($type-test tname a)))))))))

  (define-syntax fixnum?
    (type-test TT_FIXNUM))

  (define-syntax null?
    (type-test TT_NULL))

  (define-syntax list?
    (type-test TT_LIST))

  (define-syntax pair?
    (type-test TT_PAIR))

  (define-syntax procedure?
    (type-test TT_PROCEDURE))

  (define-syntax boolean?
    (type-test TT_BOOLEAN))

  (define-syntax char?
    (type-test TT_CHAR))

  (define-syntax symbol?
    (type-test TT_SYMBOL))

  (define-syntax struct?
    (type-test TT_STRUCT))

  (define-syntax string?
    (type-test TT_STRING))

  (define-syntax bytevector?
    (type-test TT_BYTEVECTOR))

  (define-syntax eof-object?
    (type-test TT_EOF))

  )
