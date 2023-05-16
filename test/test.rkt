#lang racket
(require "../compile.rkt"
         "../parse.rkt"
         rackunit)

(define (run e)
  (let ([compiled-program-string (compile (parse e))])
    (with-output-to-string
     (lambda () (system (string-append "echo \"" compiled-program-string "\"| node"))))))

(define (run-and-get-only-exit-code e)
  (let ([compiled-program-string (compile (parse e))])
    (with-output-to-string (lambda ()
                             (system (string-append "echo \""
                                                    compiled-program-string
                                                    "\"| node > /dev/null 2>&1 ; echo $?"))))))

(define (check-compile-time-error expression expected-error-string)
  (with-handlers ([exn:fail? (lambda (exn)
                               (if (string=? expected-error-string (exn-message exn)) #t #f))])
    (compile (parse expression))))

;; abscond tests
(check-equal? (run '(35)) "35\n")
(check-equal? (run '(20)) "20\n")

; blackmail tests
(check-equal? (run '((add1 35))) "36\n")
(check-equal? (run '((add1 -1))) "0\n")
(check-equal? (run '((add1 -35))) "-34\n")

(check-equal? (run '((add1 (sub1 35)))) "35\n")
(check-equal? (run '((sub1 (sub1 1)))) "-1\n")
(check-equal? (run '((sub1 (add1 0)))) "0\n")
(check-equal? (run '((sub1 -3))) "-4\n")

;; con tests
(check-equal? (run '((if (zero? (sub1 -3)) 1 2))) "2\n")
(check-equal? (run '((if (zero? (sub1 1)) 4 7))) "4\n")

(check-equal? (run '((if (zero? (sub1 -3)) 1 (add1 3)))) "4\n")
(check-equal? (run '((if (zero? (sub1 1)) (if (zero? 1) 10 5) 7))) "5\n")
(check-equal? (run '((add1 (sub1 (if (zero? (sub1 1)) (if (zero? 1) 10 5) 7))))) "5\n")

;; dupe tests
(check-equal? (run '(#f)) "#f\n")
(check-equal? (run '(#t)) "#t\n")
(check-equal? (run '((if #t 4 7))) "4\n")
(check-equal? (run '((if #f 4 7))) "7\n")
(check-equal? (run '((if 10 4 7))) "4\n")

(check-equal? (run '((if #t #f #t))) "#f\n")
(check-equal? (run '((if #f #f #t))) "#t\n")

;; evildoer tests (p.s. it was proving difficult to write tests for read-byte, so that is skipped for now)
;; but read-byte functionality can be checked by hand
(check-equal? (run '((write-byte 97))) "a\n")
(check-equal? (run '(42)) "42\n")
(check-equal? (run '((if (zero? (add1 (sub1 (add1 -1)))) 5 6))) "5\n")

;; extort tests
(check-equal? (run-and-get-only-exit-code '((sub1 (if #f 1 #f)))) "1\n")
(check-equal? (run-and-get-only-exit-code '((sub1 (if #t 3 #f)))) "0\n")
(check-equal? (run '((sub1 (if #t 3 #f)))) "2\n")
(check-equal? (run-and-get-only-exit-code '((add1 (if #f 1 #f)))) "1\n")

;; fraud tests
(check-equal? (run '((let ([x 3]) x))) "3\n")
(check-equal? (run '((let ([x 3]) (add1 x)))) "4\n")
(check-equal? (run '((let ([x 3]) (let ([y 5]) (+ x y))))) "8\n")
(check-equal? (run '((let ([x 3]) (+ x (let ([y 5]) y))))) "8\n")
(check-equal? (check-compile-time-error '(x) "undefined variable: 'x") #t)
(check-equal? (check-compile-time-error '((let ([x 3]) y)) "undefined variable: 'y") #t)
(check-equal? (check-compile-time-error '((let ([x y]) 3)) "undefined variable: 'y") #t)

;; More complex let bindings
(check-equal? (run '((let ([x 3]) (let ([x 10]) x)))) "10\n")
(check-equal? (run '((let ([x 3]) (let ([x 5]) (+ x x))))) "10\n")
(check-equal? (run '((let ([x 3]) (let ([y 5]) (+ x (+ (let ([x 10]) x) y)))))) "18\n")
(check-equal? (run '((let ([x #f]) (if x (let ([x 10]) (+ x x)) (let ([x 11]) (+ x x)))))) "22\n")

;; Test if error conditions work
(check-equal? (run-and-get-only-exit-code '((let ([x (add1 #f)]) (let ([x 10]) x)))) "1\n")
(check-equal? (run-and-get-only-exit-code '((let ([x (add1 2)]) (let ([x 10]) (add1 #t))))) "1\n")

;; hustle tests (heaps and lists)
(check-equal? (run '((box 4))) "'#&4\n")
(check-equal? (run '((cons (box 3) (cons 1 '())))) "'(#&3 1)\n")
(check-equal? (run '((car (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))) "1\n")
(check-equal? (run '((car (cons #f (cons 2 (cons 3 (cons 4 (cons 5 '())))))))) "#f\n")
(check-equal? (run '((cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))) "'(2 3 4 5)\n")
(check-equal? (run '((unbox (box (unbox (box 4)))))) "4\n")
(check-equal? (run '((let ([a (+ 1 (+ 2 3))]) (if (cons? a) 3 a)))) "6\n")
(check-equal? (run '((let ([a (cons 1 (cons 2 '()))]) (if (cons? a) 3 a)))) "3\n")

(check-equal? (run '((let ([a (cons 1 (cons 2 '()))]) (if (cons? a) (cdr a) (car a))))) "'(2)\n")
(check-equal? (run '((let ([a (cons 1 (cons 2 '()))]) (if (char? a) (cdr a) (car a))))) "1\n")

(check-equal? (run-and-get-only-exit-code '((unbox 42))) "1\n")
(check-equal? (run-and-get-only-exit-code '((car '()))) "1\n")
(check-equal? (run-and-get-only-exit-code '((cdr '()))) "1\n")
(check-equal?
 (run-and-get-only-exit-code '((let ([a (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))])
                                 (cdr (cdr (cdr (cdr (cdr (cdr a)))))))))
 "1\n")

;; hoax tests (vectors and strings)
(check-equal? (run '((let ([v (make-vector 3 #t)])
                       (begin
                         (vector-set! v 1 #f)
                         v))))
              "'#(#t #f #t)\n")
(check-equal? (run '((let ([v (make-vector 3 #t)])
                       (begin
                         (vector-set! v 1 #f)
                         v))))
              "'#(#t #f #t)\n")

(check-equal? (run '((let ([v (make-vector 3 #t)])
                       (begin
                         (vector-set! v 1 #f)
                         (vector? v)))))
              "#t\n")
(check-equal? (run '((let ([v (make-vector 3 #t)])
                       (begin
                         (vector-set! v 1 #f)
                         (vector? 10)))))
              "#f\n")

;; Tests from course langs

;; Abscond examples
(check-equal? (run '(7)) "7\n")
(check-equal? (run '(-8)) "-8\n")

;; Blackmail examples
(check-equal? (run '((add1 (add1 7)))) "9\n")
(check-equal? (run '((add1 (sub1 7)))) "7\n")

;; Con examples
(check-equal? (run '((if (zero? 0) 1 2))) "1\n")
(check-equal? (run '((if (zero? 1) 1 2))) "2\n")
(check-equal? (run '((if (zero? -7) 1 2))) "2\n")
(check-equal? (run '((if (zero? 0) (if (zero? 1) 1 2) 7))) "2\n")
(check-equal? (run '((if (zero? (if (zero? 0) 1 0)) (if (zero? 1) 1 2) 7))) "7\n")

;; Dupe examples
(check-equal? (run '(#t)) "#t\n")
(check-equal? (run '(#f)) "#f\n")
(check-equal? (run '((if #t 1 2))) "1\n")
(check-equal? (run '((if #f 1 2))) "2\n")
(check-equal? (run '((if 0 1 2))) "2\n")
(check-equal? (run '((if #t 3 4))) "3\n")
(check-equal? (run '((if #f 3 4))) "4\n")
(check-equal? (run '((if 0 3 4))) "4\n")
(check-equal? (run '((zero? 4))) "#f\n")
(check-equal? (run '((zero? 0))) "#t\n")

;; Dodger examples
(check-equal? (run '(#\a)) "#\\a\n")
(check-equal? (run '(#\b)) "#\\b\n")
(check-equal? (run '((char? #\a))) "#t\n")
(check-equal? (run '((char? #t))) "#f\n")
(check-equal? (run '((char? 8))) "#f\n")
(check-equal? (run '((char->integer #\a))) "97\n")
(check-equal? (run '((integer->char 955))) "#\\λ\n")

;; Fraud examples
(check-equal? (run '((let ([x 7]) x))) "7\n")
(check-equal? (run '((let ([x 7]) 2))) "2\n")
(check-equal? (run '((let ([x 7]) (add1 x)))) "8\n")
(check-equal? (run '((let ([x (add1 7)]) x))) "8\n")
(check-equal? (run '((let ([x 7]) (let ([y 2]) x)))) "7\n")
(check-equal? (run '((let ([x 7]) (let ([x 2]) x)))) "2\n")
(check-equal? (run '((let ([x 7]) (let ([x (add1 x)]) x)))) "8\n")

(check-equal? (run '((let ([x 0]) (if (zero? x) 7 8)))) "7\n")
(check-equal? (run '((let ([x 1]) (add1 (if (zero? x) 7 8))))) "9\n")
(check-equal? (run '((+ 3 4))) "7\n")
(check-equal? (run '((- 3 4))) "-1\n")
(check-equal? (run '((+ (+ 2 1) 4))) "7\n")
(check-equal? (run '((+ (+ 2 1) (+ 2 2)))) "7\n")
(check-equal? (run '((let ([x (+ 1 2)]) (let ([z (- 4 x)]) (+ (+ x x) z))))) "7\n")
(check-equal? (run '((= 5 5))) "#t\n")
(check-equal? (run '((= 4 5))) "#f\n")
(check-equal? (run '((= (add1 4) 5))) "#t\n")
(check-equal? (run '((< 5 5))) "#f\n")
(check-equal? (run '((< 4 5))) "#t\n")
(check-equal? (run '((< (add1 4) 5))) "#f\n")
;; Hustle examples
(check-equal? (run '((cons 1 2))) "'(1 . 2)\n")
(check-equal? (run '((unbox (box 1)))) "1\n")
(check-equal? (run '((car (cons 1 2)))) "1\n")
(check-equal? (run '((cdr (cons 1 2)))) "2\n")
(check-equal? (run '((cons 1 '()))) "'(1)\n")
(check-equal? (run '((let ([x (cons 1 2)])
                       (begin
                         (cdr x)
                         (car x)))))
              "1\n")
(check-equal? (run '((let ([x (cons 1 2)]) (let ([y (box 3)]) (unbox y))))) "3\n")
(check-equal? (run '((eq? 1 1))) "#t\n")
(check-equal? (run '((eq? 1 2))) "#f\n")
(check-equal? (run '((eq? (cons 1 2) (cons 1 2)))) "#f\n")
(check-equal? (run '((let ([x (cons 1 2)]) (eq? x x)))) "#t\n")

;; Hoax examples
(check-equal? (run '((make-vector 0 0))) "'#()\n")
(check-equal? (run '((make-vector 1 0))) "'#(0)\n")
(check-equal? (run '((make-vector 3 0))) "'#(0 0 0)\n")
(check-equal? (run '((make-vector 3 5))) "'#(5 5 5)\n")
(check-equal? (run '((vector? (make-vector 0 0)))) "#t\n")
(check-equal? (run '((vector? (cons 0 0)))) "#f\n")
(check-equal? (run '((vector-ref (make-vector 3 5) 0))) "5\n")
(check-equal? (run '((vector-ref (make-vector 3 5) 1))) "5\n")
(check-equal? (run '((vector-ref (make-vector 3 5) 2))) "5\n")

(check-equal? (run '((let ([x (make-vector 3 5)])
                       (begin
                         (vector-set! x 0 4)
                         x))))
              "'#(4 5 5)\n")
(check-equal? (run '((let ([x (make-vector 3 5)])
                       (begin
                         (vector-set! x 1 4)
                         x))))
              "'#(5 4 5)\n")
(check-equal? (run '((vector-length (make-vector 3 #f)))) "3\n")
(check-equal? (run '((vector-length (make-vector 0 #f)))) "0\n")
(check-equal? (run '("")) "\"\"\n")
(check-equal? (run '("fred")) "\"fred\"\n")
(check-equal? (run '("wilma")) "\"wilma\"\n")
(check-equal? (run '((make-string 0 #\f))) "\"\"\n")
(check-equal? (run '((make-string 3 #\f))) "\"fff\"\n")
(check-equal? (run '((make-string 3 #\g))) "\"ggg\"\n")
(check-equal? (run '((string-length ""))) "0\n")
(check-equal? (run '((string-length "fred"))) "4\n")
(check-equal? (run '((string-ref "fred" 0))) "#\\f\n")
(check-equal? (run '((string-ref "fred" 1))) "#\\r\n")
(check-equal? (run '((string-ref "fred" 2))) "#\\e\n")
(check-equal? (run '((string? "fred"))) "#t\n")
(check-equal? (run '((string? (cons 1 2)))) "#f\n")
(check-equal? (run '((begin
                       (make-string 3 #\f)
                       (make-string 3 #\f))))
              "\"fff\"\n")

;; Iniquity tests
(check-equal? (run '((define (f x)
                       x)
                     (f 5)))
              "5\n")

(check-equal? (run '((define (f x acc)
                       (if (eq? x 0) acc (f (- x 1) (+ acc x))))
                     (f 100 0)))
              "5050\n")

(check-equal? (run '((define (f x)
                       x)
                     (define (g x)
                       (f (add1 x)))
                     (define (h x)
                       (g (add1 x)))
                     (h 7)))
              "9\n")

(check-equal? (run '((define (f x)
                       x)
                     (f 5)))
              "5\n")
(check-equal? (run '((define (f x)
                       x)
                     (f 5)))
              "5\n")

(check-equal? (run '((define (tri x)
                       (if (zero? x) 0 (+ x (tri (sub1 x)))))
                     (tri 9)))
              "45\n")
