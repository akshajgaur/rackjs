#lang racket
(require "../compile.rkt" "../parse.rkt" rackunit)

(define (run e)
  (let ((compiled-program-string (compile (parse e))))
    (with-output-to-string 
      (lambda () 
        (system 
          (string-append "echo \"" compiled-program-string "\"| node"))))))

(check-equal? (run '(35)) "35\n")
(check-equal? (run '(20)) "20\n")
