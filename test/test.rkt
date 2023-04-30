#lang racket
(require "../compile.rkt" "../parse.rkt" rackunit)

(define (run e)
  (let ((compiled-program-string (compile (parse e))))
    (with-output-to-string 
      (lambda () 
        (system 
          (string-append "echo \"" compiled-program-string "\"| node"))))))


;; abscond tests
(check-equal? (run '(35)) "35\n")
(check-equal? (run '(20)) "20\n")


;; blackmail tests
(check-equal? (run '((add1 35))) "36\n")
(check-equal? (run '((add1 -1))) "0\n")
(check-equal? (run '((add1 -35))) "-34\n")

(check-equal? (run '((sub1 35))) "34\n")
(check-equal? (run '((sub1 1))) "0\n")
(check-equal? (run '((sub1 0))) "-1\n")
(check-equal? (run '((sub1 -3))) "-4\n")

;; con tests
(check-equal? (run '((if (zero? (sub1 -3)) 1 2))) "2\n")
(check-equal? (run '((if (zero? (sub1 1)) 4 7))) "4\n")

;; dupe tests
(check-equal? (run '(#f)) "false\n")
(check-equal? (run '(#t)) "true\n")
(check-equal? (run '((if #t 4 7))) "4\n")
(check-equal? (run '((if #f 4 7))) "7\n")

;; evildoer tests (p.s. it was proving difficult to write tests for read-byte, so that is skipped for now)
;; but read-byte functionality can be checked by hand
(check-equal? (run '((write-byte 4))) "4\n")
(check-equal? (run '(42)) "42\n")
(check-equal? (run '((if (zero? (add1 (sub1 (add1 -1)))) 5 6))) "5\n")
