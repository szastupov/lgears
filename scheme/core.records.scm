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
(library (core.records)
  (export make-record-type-descriptor record-type-descriptor?
          make-record-constructor-descriptor
          record-constructor-descriptor? record-constructor
          record-predicate record-accessor record-mutator record?
          record-rtd record-type-name record-type-parent
          record-type-uid record-type-sealed? record-type-opaque?
          record-type-field-names)
  (import (core.forms)
          (core.exceptions)
          (core.sequence)
          (core.arithmetic))

  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (assert (symbol? name))
    (assert (or (record-type-descriptor? parent) (boolean? parent)))
    (assert (or (boolean? uid) (symbol? uid)))
    (assert (boolean? sealed?))
    (assert (boolean? opaque?))
    (assert (vector? fields))
    (vector 'rtd name parent uid sealed? opaque? fields))

  (define (record-type-descriptor? rtd)
    (and (vector? rtd)
         (eq? (vector-ref rtd 0) 'rtd)))


  (define (record? obj)
    (and (vector? obj)
         (eq? (vector-ref obj 0) 'record)))

  (define (record-rtd rtd)
    (assert (record? rtd))
    (vector-ref rtd 1))

  (define (rtd-ref rtd idx)
    (assert (record-type-descriptor? rtd))
    (vector-ref rtd idx))

  (define (record-type-name rtd)
    (rtd-ref rtd 1))

  (define (record-type-parent rtd)
    (rtd-ref rtd 2))

  (define (record-type-uid rtd)
    (rtd-ref rtd 3))

  (define (record-type-sealed? rtd)
    (rtd-ref rtd 4))

  (define (record-type-opaque? rtd)
    (rtd-ref rtd 5))

  (define (record-type-field-names rtd)
    (vector-map cdr (rtd-ref rtd 6)))

  (define (make-record-constructor-descriptor rtd pcd proto)
    (assert (record-type-descriptor? rtd))
    (assert (or (boolean? proto) (procedure? proto)))
    (assert (or (boolean? pcd) (record-constructor-descriptor? pcd)))
    (vector 'rcd rtd pcd (if proto
                             proto
                             (lambda (p)
                               (lambda fields-values
                                 (apply p fields-values))))))

  (define (record-constructor-descriptor? rcd)
    (and (vector? rcd)
         (eq? (vector-ref rcd 0) 'rcd)))

  (define (rtd-fields rtd)
    (vector-ref rtd 6))

  (define (record-constructor rcd)
    (assert (record-constructor-descriptor? rcd))
    ((vector-ref rcd 3)
     (lambda fields-values
       (let* ((rtd (vector-ref rcd 1))
              (fields (rtd-fields rtd))
              (name (vector-ref rtd 1)))
                                        ;(assert (= (length fields-values) (vector-length fields)))
         (apply vector (append (list 'record rtd) fields-values))))))

  (define (record-predicate rtd)
    (assert (record-type-descriptor? rtd))
    (lambda (obj)
      (and (record? obj)
           (eq? (vector-ref obj 1)
                rtd))))

  (define (record-accessor rtd k)
    (assert (record-type-descriptor? rtd))
    (let ((fields (rtd-fields rtd))
          (pred? (record-predicate rtd)))
                                        ;(assert (and (>= k 0)
                                        ;             (< k (vector-length fields))))
      (lambda (rec)
        (assert (pred? rec))
        (vector-ref rec (+ 2 k)))))

  (define (record-type-mutable rtd k)
    (assert (record-type-descriptor? rtd))
    (let ((fields (rtd-fields rtd))
          (pred? (record-predicate rtd)))
                                        ;(assert (and (>= k 0)
                                        ;             (< k (vector-length fields))))
      (car (vector-ref fields k))))

  (define (record-mutator rtd k)
    (assert (record-type-descriptor? rtd))
    (let ((fields (rtd-fields rtd))
          (pred? (record-predicate rtd)))
                                        ;(assert (and (>= k 0)
                                        ;             (< k (vector-length fields))))
      (let ((mutable? (car (vector-ref fields k))))
        (lambda (rec val)
          (assert (pred? rec))
          (assert mutable?)
          (vector-set! rec (+ 2 k) val)))))

  )