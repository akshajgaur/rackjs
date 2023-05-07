#lang racket
(provide (all-defined-out))
(require "ast.rkt" "utils.rkt")
  
; TODO: need to make this so that it can take a list of params and print them
; out comma-separated
(define (arg-list-to-string arg-list) 
    (match arg-list 
        ['() ""]
        [(cons arg '()) 
            (match arg 
                [(? symbol?) (symbol->string arg)])]
        [(cons arg rest-args) 
            (match arg 
                [(? symbol?) (string-append (compile-value arg) ", " (arg-list-to-string rest-args))])]))
  ;; Arg -> String
(define (compile-value a)
    (match a
        [(? integer?) (number->string a)]
        [(? symbol?) (symbol->string a)]
        [(? boolean?) (if (equal? a #t) "true" "false")]
        [(? char?) (format-str "'%s'" (string a))]
        ['() "[]"]
        [e (error "cannot compile value " e)]))

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (string-append prequel 
                    (compile-defines ds)
                    ; (compile-define (Defn 'entry '() e))
                    "async function entry () {\n"
                    "return "
                    (compile-e e '())
                    ";\n}\n\n"
                    file-conclusion
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
      (compile-e body args))]))

(define (compile-prim0 p c)
  (match p 
    ['read-byte (format-str "(await prompt('')).charCodeAt(0)")]
  ))

;; TODO: fix this, it currently duplicates any side effects
(define (assert-type compiled-value type-str operation)
  (string-append (format-str "(typeof (%s) == '%s' ? (%s) : (%s))" compiled-value type-str operation "throwError()")))

(define (assert-type-and-perform-operation-op1 expression type-str operation cenv)
  (let ((temp-var (gensym 'typecheckvar)))
    (format-str "((%s) => {return ((typeof %s == '%s') ? (%s) : throwError() )})(%s)"
      (compile-value temp-var) 
      (compile-value temp-var) 
      type-str 
      (format-str "%s%s" (compile-value temp-var) operation)
      (compile-e expression cenv))))

(define (assert-type-and-perform-operation-op2 e1 e2 type-str operation cenv)
  (let ((temp-var1 (gensym 'typecheckvar)) (temp-var2 (gensym 'typecheckvar)))
    (format-str "((%s, %s) => {return ((typeof %s == '%s' && typeof %s == '%s') ? (%s) : throwError() )})(%s, %s)"
      (compile-value temp-var1) 
      (compile-value temp-var2)
      (compile-value temp-var1)
      type-str 
      (compile-value temp-var2)
      type-str 
      (format-str "%s %s %s" (compile-value temp-var1) operation (compile-value temp-var2))
      (compile-e e1 cenv)
      (compile-e e2 cenv)
      )))

(define (assert-cons-and-perform-operation expression operation cenv)
  (let ((temp-var (gensym 'typecheckvar)))
      (format-str "((%s) => {return (typeCheckHigherOrder(%s, '%s') ? (%s) : throwError() )})(%s)"
        (compile-value temp-var) 
        (compile-value temp-var) 
        "cons"
        (format-str "%s.value%s" (compile-value temp-var) operation)
        (compile-e expression cenv))))

(define (create-vector vector-length element cenv)
  (let ((temp-var (gensym 'typecheckvar)))
      (format-str "((%s) => {return (typeof (%s) === 'number' ? (%s) : throwError() )})(%s)"
        (compile-value temp-var) 
        (compile-value temp-var) 
        (format-str "makeVector(%s, %s)" (compile-value temp-var) (compile-e element cenv))
        (compile-e vector-length cenv))))

(define (assert-box-and-perform-operation expression operation cenv)
  (let ((temp-var (gensym 'typecheckvar)))
    (format-str "((%s) => {return typeCheckHigherOrder(%s, '%s') ? (%s) : throwError() })(%s)"
      (compile-value temp-var) 
      (compile-value temp-var) 
      "box"
      (format-str "%s%s" (compile-value temp-var) operation)
      (compile-e expression cenv))))

(define (vector-ref vector-expression index cenv)
  (let ((vector-name (gensym 'typecheckvar)) (index-value (gensym 'typecheckvar)))
    (format-str "((%s, %s) => {return (typeCheckHigherOrder(%s, '%s') && typeof (%s) === 'number' && (%s.value.length > %s && %s >= 0)) ? (%s) : throwError() })(%s, %s)"
      (compile-value vector-name) 
      (compile-value index-value) 
      (compile-value vector-name) 
      "vector"
      (compile-value index-value) 
      (compile-value vector-name) 
      (compile-value index-value) 
      (compile-value index-value) 
      (format-str "%s.value[%s]" (compile-value vector-name) (compile-value index-value))
      (compile-e vector-expression cenv)
      (compile-e index cenv)
      )))

(define (vector-set vector-expression index new-value cenv)
  (let ((vector-name (gensym 'typecheckvar)) (index-value (gensym 'typecheckvar)) (filler-parameter (gensym 'preventdefaultreturn)))
    (format-str "((%s, %s) => {return (typeCheckHigherOrder(%s, '%s') && typeof (%s) === 'number' && (%s.value.length > %s && %s >= 0)) ? (%s) : throwError() })(%s, %s)"
      (compile-value vector-name) 
      (compile-value index-value) 
      (compile-value vector-name) 
      "vector"
      (compile-value index-value) 
      (compile-value vector-name) 
      (compile-value index-value) 
      (compile-value index-value) 
      (format-str "((%s) => {return undefined;})(%s.value[%s] = (%s))" (compile-value filler-parameter) (compile-value vector-name) (compile-value index-value) (compile-e new-value cenv))
      (compile-e vector-expression cenv)
      (compile-e index cenv)
      )))

(define (compile-prim1 p e c)
  (match p 
  ['add1 (assert-type-and-perform-operation-op1 e "number" " + 1" c)]
  ['sub1 (assert-type-and-perform-operation-op1 e "number" " - 1" c)]
  ['zero? (assert-type-and-perform-operation-op1 e "number" " === 0" c)]
  ; TODO: assert that this is an integer
  ['write-byte (format-str "console.log(String.fromCharCode(%s));" (compile-e e c))]
  ['box (format-str "{ type: 'box', value: (%s) }" (compile-e e c))]
  ['unbox (assert-box-and-perform-operation e ".value" c)]
  ['car (assert-cons-and-perform-operation e "[0]" c)]
  ['cdr (assert-cons-and-perform-operation e "[1]" c)]
  ['vector? (format-str "isVector(%s)" (compile-e e c))]
))

(define (compile-prim2 p e1 e2 c)
  (match p 
    ['cons (format-str "{type: 'cons', value: [ (%s), (%s) ]}" (compile-e e1 c) (compile-e e2 c))]
    ['+ (assert-type-and-perform-operation-op2 e1 e2 "number" "+" c)]
    ['- (assert-type-and-perform-operation-op2 e1 e2 "number" "-" c)]
    ['< (assert-type-and-perform-operation-op2 e1 e2 "number" "<" c)]
    ['> (assert-type-and-perform-operation-op2 e1 e2 "number" ">" c)]
    ['= (assert-type-and-perform-operation-op2 e1 e2 "number" "===" c)]
    ['eq? (format-str "%s === %s" (compile-e e1 c) (compile-e e2 c))]
    ['make-vector (create-vector e1 e2 c)]
    ['vector-ref (vector-ref e1 e2 c)]
))

(define (compile-prim3 p e1 e2 e3 c)
  (match p 
    ['vector-set! (vector-set e1 e2 e3 c)]
))

(define (compile-if e1 e2 e3 c) (format-str "((%s) ? (%s) : (%s))" (compile-e e1 c) (compile-e e2 c) (compile-e e3 c)))

(define (compile-let varname value inner-expression cenv)
  (format-str "((%s) => {return (%s)})(%s)"
    (compile-value varname) 
    (compile-e inner-expression (cons varname cenv))
    (compile-e value cenv)))

(define (compile-variable varname cenv)
  (begin 
    (lookup varname cenv) 
    (compile-value varname)))

(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (lookup x rest)])]))

(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Var x)            (compile-variable x c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]  
    [(Empty)            (compile-value '())] 
    ['() ""]
    [_                  (error "Not yet implemented")]
    ; Cut off everything that has not been implemented yet
    
    ; [(Eof)              (compile-value eof)]
    ; [(Str s)            (compile-string s)]
    
    ; [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ))
