(import (rnrs)
        (lgears format))

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
