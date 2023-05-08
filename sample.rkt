#lang racket
; (let ((x (read-byte))) (add1 x))
; (add1 (add1 (read-byte)))
; (unbox (box 42))
; (unbox (read-byte))
; (if (= 98 (read-byte)) (+ 1 (+ 2 3)) (- 1000 #f))
; (< 2 1)
; (make-vector 4 1)
; (let ((a (box 54))) (unbox (unbox a)))
; (cdr (cons 1 (cons 2 '())))

; (let ((a 5)) (if (vector? a) a -1))

; (let ((a (make-vector 5 1))) (begin (vector-set! a 2 20) a))
; (let ((a (vector-set! (make-vector 5 1) 2 20))) a)
; (let ((a (make-vector 5 1))) a)

; (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))
; (cons 1 '())
; '()

; (cons 1 (cons 2 3))
; (cons '() '())
; (cons (cons '() '()) (cons '() '()))
; (cons (cons 1 2) 3)

; (make-vector 3 (cons 1 2))

(let ((a (make-vector 5 1))) (begin (vector-set! a 1 (cons 1 (cons 2 #f))) a))
