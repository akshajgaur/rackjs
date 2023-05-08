#lang racket
(provide (all-defined-out))

(define reserved-funs  
        (list "break" "case" "catch" "continue" "debugger" "default" 
        "delete" "do" "else" "finally" "for" "function" "if" "in" 
        "instanceof" "new" "return" "switch" "this" "throw" "try" 
        "typeof" "var" "void" "while" "with"))

(define (format-str str . xs)
    (match xs
    ['() (if (string-contains? str "%s") (error "Too many format specifiers!") str)]
    [(cons x rest) (apply format-str (string-replace str "%s" x #:all? #false) rest)]))

(define higher-order-type-checking (string-append "function typeCheckHigherOrder(thing, type) {\n"
                                                "\tif (typeof thing === 'object' && thing.type == type) {\n"
                                                    "\t\treturn true;\n"
                                                "\t} else {\n"
                                                    "\t\tthrow new Error('err');\n"
                                                "\t}\n"
                                            "}\n\n"
))

(define node-imports (string-append "const readline = require('readline');\n\n"))
(define js-readline (string-append "function prompt(question) {\n"
                                    "\tconst rl = readline.createInterface({\n"
                                        "\t\tinput: process.stdin,\n"
                                        "\t\toutput: process.stdout\n"
                                    "\t});\n"

                                    "\treturn new Promise(resolve => {\n"
                                        "\t\trl.question(question, answer => {\n"
                                        "\t\t\trl.close();\n"
                                        "\t\t\tresolve(answer);\n"
                                        "\t\t});\n"
                                    "\t});\n"
                                    "}\n\n"
                                    "function throwError() {\n"
                                        "\tthrow new Error('err');\n"
                                    "}\n\n"
))

(define make-vector-function (string-append "function makeVector(length, element) {\n"
                                                "\tif (length < 0) {throwError()}\n"
                                                "\tarr = [];\n"
                                                "\tfor (let i = 0; i < length; i ++) {\n"
                                                    "\t\tarr.push(element);\n"
                                                "\t}\n"
                                                "\treturn {type: 'vector', value: arr};\n"
                                            "}\n"
))

(define vector?-function (string-append "function isVector(element) {\n"
                                                "\treturn (typeof element === 'object') && element.type == 'vector';\n"
                                            "}\n"
))

(define format-output-function (string-append "function correctPrint(value, outermostElement, secondElementOfCons) {\n"
    "\tif (typeof value === 'object' && value.type === 'emptycons') {\n"
        "\t\tif (outermostElement) {\n"
            "\t\t\treturn \"'()\";\n"
        "\t\t}\n"
        "\t\tif (secondElementOfCons) {\n"
            "\t\t\treturn \"\";\n"
        "\t\t}\n"
        "\t\treturn \"()\";\n"
    "\t} else if (value === true) {\n"
        "\t\tif (secondElementOfCons) {\n"
            "\t\t\treturn \" . #t\";\n"
        "\t\t}\n"
        "\t\treturn \"#t\";\n"
    "\t} else if (value === false) {\n"
        "\t\tif (secondElementOfCons) {\n"
            "\t\t\treturn \" . #f\";\n"
        "\t\t}\n"
        "\t\treturn \"#f\";\n"
    "\t} else if (typeof value === 'number') {\n"
        "\t\tif (secondElementOfCons) {\n"
            "\t\t\treturn ` . ${value}`;\n"
        "\t\t}\n"
        "\t\treturn `${value}`;\n"
    "\t} else if (typeof value === 'object' && value.type === 'cons') {\n"
        "\t\tif (outermostElement) {\n"
            "\t\t\treturn `'(${correctPrint(value.value[0], false, false)}${correctPrint(value.value[1], false, true)})`;\n"
        "\t\t} else {\n"
            "\t\t\tif (secondElementOfCons) {\n"
                "\t\t\t\tif (typeof value === 'object' && value.type === 'cons') {\n"
                    "\t\t\t\t\treturn ` ${correctPrint(value.value[0], false, false)}${correctPrint(value.value[1], false, true)}`;\n"
                "\t\t\t\t}\n"
            "\t\t\t} else {\n"
                "\t\t\t\treturn `(${correctPrint(value.value[0], false, false)}${correctPrint(value.value[1], false, true)})`;\n"
            "\t\t\t}\n"
        "\t\t}\n"
    "\t} else if (typeof value === 'object' && value.type === 'vector') {\n"
        "\t\tlet final_string = '';\n"
        "\t\tif (outermostElement) {\n"
            "\t\t\tfinal_string = final_string + \"'\";\n"
        "\t\t}\n"
        "\t\tfinal_string = final_string + '#(';\n"
        "\t\tfor (let i = 0; i < value.value.length - 1; i ++) {\n"
            "\t\t\tfinal_string = final_string + `${correctPrint(value.value[i], false, false)} `;\n"
        "\t\t}\n"
        "\t\tif (value.value.length > 0) {\n"
            "\t\t\tfinal_string = final_string + `${correctPrint(value.value[value.value.length - 1])})`;\n"
        "\t\t} else {\n"
            "\t\t\tfinal_string = final_string + ')';\n"
        "\t\t}\n"
        "\t\treturn final_string;\n"
    "\t} else {\n"
        "\t\treturn value;\n"
    "\t}\n"
"}\n\n"
))

(define prequel (string-append node-imports js-readline higher-order-type-checking make-vector-function vector?-function format-output-function))

(define file-conclusion (string-append  "entry().then((returnvalue) => {\n"
                                            "\treturnvalue == undefined ? '' : console.log(correctPrint(returnvalue, true, false));\n"
                                        "})\n"
))


(define (valid-keyword fun) 
   (match (member fun reserved-funs)
    [#f #t]
    [_ #f]
   )
)

(define (valid-first-char fun) 
  (let ((l (car (string->list fun))))  
  
    (or (char-alphabetic? l) (eq? #\$ l) (eq? #\_ l)))
  
)

(define (valid-last-char fun) 
  (let ((l (car (reverse (string->list fun)))))  
    (or (char-alphabetic? l) (char-numeric? l) (eq? #\_ l)))
  )
  
(define (valid-fun sym-fun) 

    (let ((fun (symbol->string sym-fun)))
        (and (valid-keyword fun) (valid-first-char fun) (valid-last-char fun))))
