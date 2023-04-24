#lang racket
(provide (all-defined-out))
(require "ast.rkt")
  
(define tab (make-string 8 #\space))

; TODO: need to make this so that it can take a list of params and print them
; out comma-separated
(define (arg-list-to-string arg-list) 
    (match arg-list 
        ['() ""]
        [(cons arg '()) 
            (match arg 
                [(? symbol?) (symbol->string arg)]
            )]
        [(cons arg rest-args) 
            (match arg 
                [(? symbol?) (string-append (compile-value arg) ", " (arg-list-to-string rest-args))]
            )
        ]))

  ;; Arg -> String
(define (compile-value a)
    (match a
        [(? integer?) (number->string a)]
        [(? symbol?) (symbol->string a)]
        [e (error "cannot compile value " e)]
    ))

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (string-append (compile-defines ds)
                    (compile-define (Defn 'entry '() e))
        ;    (compile-e e '())
                    ; (compile-app 'console.log (compile-app 'entry '() '()) '())
                    ; temp placeholder
                    "console.log(entry());"
           )]))
;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() ""]
    [(cons d ds)
     (string-append (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
; TODO: needs to be updated with putting args in the environment
(define (compile-define d)
  (match d
    [(Defn function-name args body)
     (string-append "function "
                        (compile-value function-name) "(" 
                        (arg-list-to-string args) ") {\n" "return "
                        (compile-e body args) ";}\n"
                        )]))

;; TODO: need to compile the arguments and put them between the parens
; right now it only accepts one arg
(define (compile-app function-name args c)
  (string-append (compile-value function-name) "(" (compile-e args c) ")"))

(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(App f es)         (compile-app f es c)]
    ['() ""]
    ; Cut off everything that has not been implemented yet
    [e                  (error "Not yet implemented" e)]
    ; [(Bool b)           (compile-value b)]
    ; [(Char c)           (compile-value c)]
    ; [(Eof)              (compile-value eof)]
    ; [(Empty)            (compile-value '())]
    ; [(Var x)            (compile-variable x c)]
    ; [(Str s)            (compile-string s)]
    ; [(Prim0 p)          (compile-prim0 p c)]
    ; [(Prim1 p e)        (compile-prim1 p e c)]
    ; [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ; [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    ; [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    ; [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ; [(Let x e1 e2)      (compile-let x e1 e2 c)]    
    ))
