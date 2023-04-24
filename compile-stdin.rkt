#lang racket
(provide main)
; (require "parse.rkt" "compile.rkt" a86/printer "ajs/printer.rkt")
; (require "parse.rkt" "compile.rkt" "ajs/printer.rkt")
; (require "parse.rkt" "compile.rkt" "ajs/rackjs-printer.rkt")
(require "parse.rkt" "compile.rkt" "read-all.rkt")

;; -> Void
;; Compile contents of stdin,
;; emit asm code on stdout
; (define (main)
;   (read-line) ; ignore #lang racket line
;   (asm-display (compile (parse (read)))))


;; -> Void
;; Compile contents of stdin,
;; emit js code on stdout
; (define (main)
;   (read-line) ; ignore #lang racket line
;   (displayln (simple-instr->string (compile (parse (read))))))


;; -> Void
;; Compile contents of stdin,
;; emit js code on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  ; (displayln (compile (parse (read))))
  (displayln (compile (parse (read-all))))
  )
