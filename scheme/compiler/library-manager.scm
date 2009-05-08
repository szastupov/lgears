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

(library (compiler library-manager)
  (export find-library library-install! library-manager-root
          find-library-file libname->path libname->symbol)
  (import (rnrs base)
          (rnrs lists)
          (rnrs files)
          (rnrs records syntactic)
          (format))

  (define library-path '("." ".."))
  (define library-extensions '("scm" "ss" "sls"))

  (define (library-manager-root)
    (make-lnode #f '()))

  (define-record-type lnode
    (fields (mutable lib)
            (mutable childs)))

  (define (lnode-new name lib childs)
    (cons name (make-lnode lib childs)))

  (define (library-search node name)
    (cond ((assq (car name) (lnode-childs node))
           => (lambda (res)
                (if (= (length name) 1)
                    (cdr res)
                    (library-search (cdr res) (cdr name)))))
          (else #f)))

  (define (find-library root name)
    (cond ((library-search root name)
           => lnode-lib)
          (else #f)))

  (define (create-path name lib)
    (list
     (let loop ((name name))
       (if (= (length name) 1)
           (lnode-new (car name) lib '())
           (lnode-new (car name)
                      #f
                      (list (loop (cdr name))))))))

  (define (library-install! root name lib)
    (cond ((library-search root name)
           => (lambda (res)
                (lnode-lib-set! res lib)))
          (else
           (let loop ((node root)
                      (name name))
             (cond ((assq (car name) (lnode-childs node))
                    => (lambda (res)
                         (loop (cdr res) (cdr name))))
                   ((= (length name) 1)
                    (lnode-childs-set!
                     node
                     (cons (lnode-new (car name) lib '())
                           (lnode-childs node))))
                   ((null? (lnode-childs node))
                    (lnode-childs-set! node (create-path name lib)))
                   (else (error 'library-install! "wtf?")))))))

  (define (libname->string name sep)
    (fold-left (lambda (prev cur)
                 (string-append prev sep (symbol->string cur)))
               (symbol->string (car name)) (cdr name)))

  (define (libname->path name)
    (libname->string name "/"))

  (define (libname->symbol name)
    (string->symbol
     (libname->string name ".")))

  (define (try-file lp path ext)
    (let ((cn (string-append lp "/" path "." ext)))
      (if (file-exists? cn) cn #f)))

  (define (find-library-file name)
    (let ((path (libname->path name)))
      (call/cc
       (lambda (return)
         (for-each
          (lambda (lp)
            (for-each
             (lambda (ext)
               (cond ((try-file lp path ext)
                      => (lambda (res)
                           (return res)))))
             library-extensions))
          library-path)
         (error 'find-library-file
                (format "library ~a not found" name))))))

  )