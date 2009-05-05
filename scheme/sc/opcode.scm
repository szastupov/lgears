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

(library (sc opcode)
  (export oplist oplist-for-each opcode object-types object-type)
  (import (rnrs))

  #|
   | DO NOT FORGET TO RUN make regen-opcode after updating it
   |#

  (define oplist
    '((LOAD_FUNC . "Load function local to module")
      (LOAD_CLOSURE . "Make closure on heap")
      (LOAD_LOCAL . "Load object from frame-local area")
      (LOAD_BIND . "Load object form binding")
      (LOAD_CONST . "Load object from module constant area")
      (PUSH_BOOL . "Push boolean value")
      (PUSH_NULL . "Load built-in constant")
      (JUMP_IF_FALSE . "Jump if false")
      (JUMP_IF_TRUE . "Jump if true")
      (JUMP_FORWARD . "Jump forward")
      (FUNC_CALL . "Call function")
      (SET_LOCAL . "Assign new value to local binding")
      (SET_BIND . "Asign new value to non-local binding")))

  (define object-types
    '((OT_FIXNUM . "Fixed number")
      (OT_CHARACTER . "Character")
      (OT_STRING . "String")
      (OT_SYMBOL . "Symbol")
      (OT_STATIC . "Static variable")))
  
  (define (oplist-for-each func)
    (fold-left (lambda (idx op)
                 (func idx op)
                 (+ idx 1))
               0 oplist))

  (define (ifind key lst)
    (let loop ((idx 0)
               (cur lst))
      (cond ((null? cur) #f)
            ((eq? (caar cur) key) idx)
            (else (loop (+ 1 idx) (cdr cur))))))

  (define (opcode x)
    (cond ((ifind x oplist)
           => (lambda (i) i))
          (else (error 'opcode "unknown opcode" x))))

  (define (object-type x)
    (cond ((ifind x object-types)
           => (lambda (i) i))
          (else (error 'object-type "unknown object type" x))))

  )
