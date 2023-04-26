#lang racket
(provide (all-defined-out))
(require "ast.rkt" "utils.rkt")

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
      (format-str "function %s (%s) {\n return %s;\n}\n"
      (compile-value function-name)
      (arg-list-to-string args)
      (compile-e body args))
    ]))


(define (compile-prim1 p e c)
  (match p 
  ['add1 (format-str "%s + %s" (compile-e e c) (compile-value 1))]
  ['sub1 (format-str "%s - %s" (compile-e e c) (compile-value 1))]
  )

)
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(App f es)         (compile-app f es c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    ['() ""]
    [_                  (error "Not yet implemented")]
    ; Cut off everything that has not been implemented yet
    
    ; [(Bool b)           (compile-value b)]
    ; [(Char c)           (compile-value c)]
    ; [(Eof)              (compile-value eof)]
    ; [(Empty)            (compile-value '())]
    ; [(Var x)            (compile-variable x c)]
    ; [(Str s)            (compile-string s)]
    ; [(Prim0 p)          (compile-prim0 p c)]
    
    ; [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ; [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    ; [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    ; [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ; [(Let x e1 e2)      (compile-let x e1 e2 c)]    
    ))
