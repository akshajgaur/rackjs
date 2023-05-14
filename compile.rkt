#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "utils.rkt"
         "fv.rkt")

#| Convert a list of symbols to its correct parameter list in JavaScript |#
(define (param-list-to-string arg-list)
  (match arg-list
    ['() ""]
    [(cons arg '())
     (match arg
       [(? symbol?) (symbol->string arg)])]
    [(cons arg rest-args)
     (match arg
       [(? symbol?) (string-append (symbol->string arg) ", " (param-list-to-string rest-args))])]))

#| Convert a list of values to its correct parameter list in JavaScript |#
(define (args-list-to-string arg-list)
  (match arg-list
    ['() ""]
    [(cons arg '()) arg]
    [(cons arg rest-args) (string-append arg ", " (args-list-to-string rest-args))]))

#| Compile a value in JavaScript |#
(define (compile-value a)
  (match a
    [(? integer?) (number->string a)]
    [(? symbol?) (symbol->string a)]
    [(? string?) (format-str "{type: 'string', value: '%s'}" a)]
    [(? boolean?) (if (equal? a #t) "true" "false")]
    [(? char?) (format-str "{type: 'char', value: '%s'}" (string a))]
    ['() "{ type: 'emptycons' }"]
    [e (error "cannot compile value " e)]))

#| Main compile function: Prog -> String |#
(define (compile p)
  (match p
    [(Prog ds e)
     (string-append prequel
                    (compile-defines ds)
                    "async function entry () {\n"
                    "\treturn "
                    (compile-e e '())
                    ";\n}\n\n"
                    file-conclusion)]))
#| Compile function definitions: [Listof Defn] -> String |#
(define (compile-defines ds)
  (match ds
    ['() ""]
    [(cons d ds) (string-append (compile-define d) (compile-defines ds))]))

#| Compile single function definition: Defn -> String |#
(define (compile-define d)
  (match d
    [(Defn function-name args body)
     (match (valid-fun function-name)
       [#t
        (format-str "function %s (%s) {\n return %s;\n}\n"
                    (compile-value function-name)
                    (param-list-to-string args)
                    (compile-e body args))]
       [#f (error "Invalid JavaScript function!")])]))

#| Compile primitives with 0 args |#
(define (compile-prim0 p c)
  (match p
    ['read-byte (format-str "(await prompt('')).charCodeAt(0)")]))

#| Add runtime type-checking to runtime-time |#
(define (assert-type compiled-value type-str operation)
  (string-append (format-str "(typeof (%s) == '%s' ? (%s) : (%s))"
                             compiled-value
                             type-str
                             operation
                             "throwError()")))

#| Peform runtime type-checking and op1 operation |#
(define (assert-type-and-perform-operation-op1 expression type-str operation cenv)
  (let ([temp-var (gensym 'typecheckvar)])
    (format-str "((%s) => {return ((typeof %s == '%s') ? (%s) : throwError() )})(%s)"
                (compile-value temp-var)
                (compile-value temp-var)
                type-str
                (format-str "%s%s" (compile-value temp-var) operation)
                (compile-e expression cenv))))

#| Peform runtime type-checking and op2 operation |#
(define (assert-type-and-perform-operation-op2 e1 e2 type-str operation cenv)
  (let ([temp-var1 (gensym 'typecheckvar)] [temp-var2 (gensym 'typecheckvar)])
    (format-str
     "((%s, %s) => {return ((typeof %s == '%s' && typeof %s == '%s') ? (%s) : throwError() )})(%s, %s)"
     (compile-value temp-var1)
     (compile-value temp-var2)
     (compile-value temp-var1)
     type-str
     (compile-value temp-var2)
     type-str
     (format-str "%s %s %s" (compile-value temp-var1) operation (compile-value temp-var2))
     (compile-e e1 cenv)
     (compile-e e2 cenv))))

#| Peform runtime type-checking for cons and output code for cons |#
(define (assert-cons-and-perform-operation expression operation cenv)
  (let ([temp-var (gensym 'typecheckvar)])
    (format-str "((%s) => {return (typeCheckHigherOrder(%s, '%s') ? (%s) : throwError() )})(%s)"
                (compile-value temp-var)
                (compile-value temp-var)
                "cons"
                (format-str "%s.value%s" (compile-value temp-var) operation)
                (compile-e expression cenv))))

#| Create vector in JavaScript |#
(define (create-vector vector-length element cenv)
  (let ([temp-var (gensym 'typecheckvar)])
    (format-str "((%s) => {return (typeof (%s) === 'number' ? (%s) : throwError() )})(%s)"
                (compile-value temp-var)
                (compile-value temp-var)
                (format-str "makeVector(%s, %s)" (compile-value temp-var) (compile-e element cenv))
                (compile-e vector-length cenv))))

#| Create string in JavaScript |#
(define (create-string vector-length element cenv)
  (let ([temp-var (gensym 'typecheckvar)])
    (format-str "((%s) => {return (typeof (%s) === 'number' ? (%s) : throwError() )})(%s)"
                (compile-value temp-var)
                (compile-value temp-var)
                (format-str "makeString(%s, %s)" (compile-value temp-var) (compile-e element cenv))
                (compile-e vector-length cenv))))

#| Perform run-time typchecking for box and generate code for operation |#
(define (assert-box-and-perform-operation expression operation cenv)
  (let ([temp-var (gensym 'typecheckvar)])
    (format-str "((%s) => {return typeCheckHigherOrder(%s, '%s') ? (%s) : throwError() })(%s)"
                (compile-value temp-var)
                (compile-value temp-var)
                "box"
                (format-str "%s%s" (compile-value temp-var) operation)
                (compile-e expression cenv))))

#| Perform run-time typchecking for vector-ref and generate code for operation |#
(define (vector-ref vector-expression index cenv)
  (let ([vector-name (gensym 'typecheckvar)] [index-value (gensym 'typecheckvar)])
    (format-str
     "((%s, %s) => {return (typeCheckHigherOrder(%s, '%s') && typeof (%s) === 'number' && (%s.value.length > %s && %s >= 0)) ? (%s) : throwError() })(%s, %s)"
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
     (compile-e index cenv))))

#| Perform run-time typchecking for string-ref and generate code for operation |#
(define (string-ref string-expression index cenv)
  (let ([string-name (gensym 'stringname)] [index-value (gensym 'indexvalue)])
    (format-str
     "((%s, %s) => {return (typeCheckHigherOrder(%s, 'string') && typeof (%s) === 'number' && (%s.value.length > %s && %s >= 0)) ? (%s) : throwError() })(%s, %s)"
     (compile-value string-name)
     (compile-value index-value)
     (compile-value string-name)
     (compile-value index-value)
     (compile-value string-name)
     (compile-value index-value)
     (compile-value index-value)
     (format-str "%s.value[%s]" (compile-value string-name) (compile-value index-value))
     (compile-e string-expression cenv)
     (compile-e index cenv))))

#| Perform run-time typchecking for vector-set and generate code for operation |#
(define (vector-set vector-expression index new-value cenv)
  (let ([vector-name (gensym 'typecheckvar)]
        [index-value (gensym 'typecheckvar)]
        [filler-parameter (gensym 'preventdefaultreturn)])
    (format-str
     "((%s, %s) => {return (typeCheckHigherOrder(%s, '%s') && typeof (%s) === 'number' && (%s.value.length > %s && %s >= 0)) ? (%s) : throwError() })(%s, %s)"
     (compile-value vector-name)
     (compile-value index-value)
     (compile-value vector-name)
     "vector"
     (compile-value index-value)
     (compile-value vector-name)
     (compile-value index-value)
     (compile-value index-value)
     (format-str "((%s) => {return undefined;})(%s.value[%s] = (%s))"
                 (compile-value filler-parameter)
                 (compile-value vector-name)
                 (compile-value index-value)
                 (compile-e new-value cenv))
     (compile-e vector-expression cenv)
     (compile-e index cenv))))

#| Compile prim1 operations |#
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
    ['vector? (format-str "isHigherOrderType(%s, 'vector')" (compile-e e c))]
    ['box? (format-str "isHigherOrderType((%s), 'box')" (compile-e e c))]
    ['cons? (format-str "isHigherOrderType((%s), 'cons')" (compile-e e c))]
    ['char? (format-str "isHigherOrderType((%s), 'char')" (compile-e e c))]))

#| Compile prim2 operations |#
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
    ['make-string (create-string e1 e2 c)]
    ['vector-ref (vector-ref e1 e2 c)]
    ['string-ref (string-ref e1 e2 c)]))

#| Compile prim3 operations |#
(define (compile-prim3 p e1 e2 e3 c)
  (match p
    ['vector-set! (vector-set e1 e2 e3 c)]))

#| Compile if expressions |#
(define (compile-if e1 e2 e3 c)
  (format-str "((%s) ? (%s) : (%s))" (compile-e e1 c) (compile-e e2 c) (compile-e e3 c)))

#| Compile let expressions |#
(define (compile-let varname value inner-expression cenv)
  (match (valid-fun varname)
    [#t
     (format-str "((%s) => {return (%s)})(%s)"
                 (compile-value varname)
                 (compile-e inner-expression (cons varname cenv))
                 (compile-e value cenv))]
    [#f (error "Invalid variable name in JavaScript!")]))

#| Compile function applications |#
(define (compile-app e es c)
  (match e
    ;; Normal application for pre-defined functions
    ;; Applications for lambdas
    [(Lam f xs e)
     (let ([fvs (fv e)])
       (format-str "((%s) => %s(%s)) ((%s) => (%s))"
                   (symbol->string f)
                   (symbol->string f)
                   (args-list-to-string (compile-es es (append c (append xs fvs))))
                   (args-list-to-string (map symbol->string xs))
                   (compile-e e (append fvs xs))))]
    [(App g e) (compile-app (App (compile-e g c) (append es e)) c)]
    
    [(Var x) (format-str "%s(%s)" (symbol->string x) (args-list-to-string (compile-es es c)))]))
(define (compile-lam f xs e c)
 (let ([fvs (fv e)])
   (format-str "((%s) => %s()) ((%s) => (%s))"
                   (symbol->string f)
                   (symbol->string f)
                   (param-list-to-string xs)
                   (compile-e e (append (append c fvs) xs)))))


#| Compile a variable |#
(define (compile-variable varname cenv)
  (begin
    (lookup varname cenv)
    (compile-value varname)))

#| Lookup a variable |#
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (lookup x rest)])]))

#| Compile multiple expressions |#
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons exp exprs) (cons (compile-e exp c) (compile-es exprs c))]))

#| Compile begin |#
(define (compile-begin e1 e2 c)
  (format-str "(%s, %s)" (compile-e e1 c) (compile-e e2 c)))

#| Compile arbritrary expression |#
(define (compile-e e c)
  (match e
    [(Int i) (compile-value i)]
    [(Bool b) (compile-value b)]
    [(Char c) (compile-value c)]
    [(Prim0 p) (compile-prim0 p c)]
    [(Prim1 p e) (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3) (compile-if e1 e2 e3 c)]
    [(Var x) (compile-variable x c)]
    [(App f es) (compile-app f es c)]
    [(Let x e1 e2) (compile-let x e1 e2 c)]
    [(Str s) (compile-value s)]
    [(Begin e1 e2) (compile-begin e1 e2 c)]
    [(Lam f xs e) (compile-lam f xs e c)]
    [(Empty) (compile-value '())]
    ['() ""]
    [n n]))
