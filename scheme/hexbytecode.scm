#|
 | This file is part of lGears scheme system
 | Copyright (C) 2009 Stepan Zastupov
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

(import (rnrs)
        (format))

(if (< (length (command-line)) 3)
  (begin
    (format #t "Usage: ~a input output\n" (car (command-line)))
    (exit 1))
  (let ((input (open-file-input-port
                 (cadr (command-line))))
        (output (open-file-output-port
                  (caddr (command-line))
                  (file-options no-fail)
                  (buffer-mode block)
                  (native-transcoder))))
    (format output "static const uint8_t image[] = {\n\t")
    (let loop ((rb (get-u8 input))
               (cnt 0))
      (if (eof-object? (lookahead-u8 input))
        (format output "0x~x\n};\n" rb)
        (begin
          (format output "0x~x, " rb)
          (loop
            (get-u8 input) 
            (if (= cnt 16)
              (begin
                (put-string output "\n\t") 0)
              (+ 1 cnt))))))
    (close-port input)
    (close-port output)))
