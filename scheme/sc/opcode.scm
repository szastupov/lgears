#|
 | Copyright (C) 2009 - Stepan Zastupov
 | This program is free software; you can redistribute it and/or
 | modify it under the terms of the GNU General Public License
 | as published by the Free Software Foundation; either version 2
 | of the License, or (at your option) any later version.
 |
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with this program; if not, write to the Free Software
 | Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 |#

(library (sc opcode)
  (export oplist oplist-for-each opcode)
  (import (rnrs))

  #|
   | DO NOT FORGET TO RUN make regen-opcode after updating it
   |#

  (define oplist '((LOAD_FUNC . "Load function local to module")
                   (LOAD_CLOSURE . "Make closure on heap")
                   (LOAD_LOCAL . "Load object from frame-local area")
                   (LOAD_BIND . "Load object form binding")
                   (LOAD_SYM . "Load predefined symbol local to module")
                   (PUSH_BOOL . "Push boolean value")
                   (PUSH_FIXNUM . "Push fixed number")
                   (LOAD_CONST . "Load built-in constant")
                   (LOAD_STRING . "Load string")
                   (LOAD_IMPORT . "Load object from module import table")
                   (JUMP_IF_FALSE . "Jump if false")
                   (JUMP_IF_TRUE . "Jump if true")
                   (JUMP_FORWARD . "Jump forward")
                   (FUNC_CALL . "Call function")
                   (SET_LOCAL . "Assign new value to local binding")
                   (SET_BIND . "Asign new value to non-local binding")))

  (define (oplist-for-each func)
    (fold-left (lambda (idx op)
                 (func idx op)
                 (+ idx 1))
               0 oplist))

  (define (make-opcode-table)
    (let ([tbl (make-eq-hashtable)])
      (oplist-for-each (lambda (idx op)
                         (hashtable-set! tbl (car op) idx)))
      tbl))

  (define optable (make-opcode-table))

  (define (opcode sym)
    (if (hashtable-contains? optable sym)
      (hashtable-ref optable sym #f)
      (error 'opcode "unknown opcode" sym)))
  )
