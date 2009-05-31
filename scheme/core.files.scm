(library (core.files)
  (export file-exists? delete-file current-directory directory-list
          make-directory)
  (import (core.forms)
          (core.exceptions))

  (define (file-exists? path)
    (if (builtin-call fs-stat path) #t #f))

  (define (delete-file path)
    (if (not (builtin-call fs-remove path))
        (error 'delete-file (describe-os-error) path)))

  (define (current-directory . arg)
    (if (null? arg)
        (builtin-call fs-getcwd)
        (builtin-call fs-chdir (car arg))))

  (define (make-directory path . mode)
    (if (null? mode)
        (builtin-call fs-mkdir path #o755)
        (builtin-call fs-mkdir path (car mode))))

  (define (call-with-dir dirpath proc)
    (let ((dir (builtin-call fs-opendir dirpath)))
      (if dir
          (let ((res (proc dir)))
            (builtin-call fs-closedir dir)
            res)
          (raise "failed to open dir"))))

  (define (directory-list dirpath)
    (call-with-dir
     dirpath
     (lambda (dir)
       (let loop ((cur (builtin-call fs-readdir dir))
                  (res '()))
         (cond ((eof-object? cur) res)
               (cur (loop (builtin-call fs-readdir dir)
                          (cons cur res)))
               (else (raise "wtf?")))))))
)