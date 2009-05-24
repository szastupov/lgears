(library (core.files)
  (export file-exists? delete-file current-directory directory-list)
  (import (core.forms)
          (core.exceptions))

  (define (file-exists? path)
    (if (fs-stat path) #t #f))

  (define (delete-file path)
    (assert (file-exists? path))
    (fs-remove path))

  (define current-directory fs-getcwd)


  (define (call-with-dir dirpath proc)
    (let ((dir (fs-opendir dirpath)))
      (if dir
          (let ((res (proc dir)))
            (fs-closedir dir)
            res)
          (raise "failed to open dir"))))

  (define (directory-list dirpath)
    (call-with-dir
     dirpath
     (lambda (dir)
       (let loop ((cur (fs-readdir dir))
                  (res '()))
         (cond ((eof-object? cur) res)
               (cur (loop (fs-readdir dir)
                          (cons cur res)))
               (else (raise "wtf?")))))))
)