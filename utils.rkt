#lang racket
(provide (all-defined-out))
#| List of reserved keywords for functions and variable names in JavaScript |#
(define reserved-funs
  (list "break"
        "case"
        "catch"
        "continue"
        "debugger"
        "default"
        "delete"
        "do"
        "else"
        "finally"
        "for"
        "function"
        "if"
        "in"
        "instanceof"
        "new"
        "return"
        "switch"
        "this"
        "throw"
        "try"
        "typeof"
        "var"
        "void"
        "while"
        "with"))

#| Create a format string given a string and parameters to the format string |#
(define (format-str str . xs)
  (match xs
    ['() (if (string-contains? str "%s") (error "Too many format specifiers!") str)]
    [(cons x rest) (apply format-str (string-replace str "%s" x #:all? #false) rest)]))

#| Function in JavaScript to check type of higher order objects |#
(define higher-order-type-checking
  (string-append "function typeCheckHigherOrder(thing, type) {\n"
                 "\tif (typeof thing === 'object' && thing.type == type) {\n"
                 "\t\treturn true;\n"
                 "\t} else {\n"
                 "\t\tthrow new Error('err');\n"
                 "\t}\n"
                 "}\n\n"))

#| Add node imports to javascript file |#
(define node-imports (string-append "const readline = require('readline');\n\n"))

#| Add boilerplate code to allow reading from standard input in JavaScript |#
(define js-readline
  (string-append "function prompt(question) {\n"
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
                 "}\n\n"))

#| Add boilerplate code to make a vector in JavaScript|#
(define make-vector-function
  (string-append "function makeVector(length, element) {\n"
                 "\tif (length < 0) {throwError()}\n"
                 "\tarr = [];\n"
                 "\tfor (let i = 0; i < length; i ++) {\n"
                 "\t\tarr.push(element);\n"
                 "\t}\n"
                 "\treturn {type: 'vector', value: arr};\n"
                 "}\n"))

#| Add boilerplate code to make a string in JavaScript |#
(define make-string-function
  (string-append "function makeString(length, element) {\n"
                 "\tif (length < 0) {throwError()}\n"
                 "if (!(typeof element === 'object' && element.type === 'char')) {throwError()}\n"
                 "\tarr = [];\n"
                 "\tfor (let i = 0; i < length; i ++) {\n"
                 "\t\tarr.push(element);\n"
                 "\t}\n"
                 "\treturn {type: 'string', value: arr};\n"
                 "}\n"))

#| Add boilerplate code to check if value is a vector in JavaScript |#
(define vector?-function
  (string-append "function isHigherOrderType(element, type) {\n"
                 "\treturn (typeof element === 'object') && element.type == type;\n"
                 "}\n"))

(define format-output-function
  (string-append
   "function correctPrint(value, outermostElement, secondElementOfCons) {\n"
   "if (typeof value === 'object' && value.type === 'char') {\n"
   "let a = String.fromCharCode(92);\n"
   "return a + value.value;\n"
   "}\n"
   "if (typeof value === 'object' && value.type === 'emptycons') {\n"
   "if (outermostElement) {\n"
   "return '\\'()';\n"
   "}\n"
   "if (secondElementOfCons) {\n"
   "return '';\n"
   "}\n"
   "return '()';\n"
   "} else if (value === true) {\n"
   "if (secondElementOfCons) {\n"
   "return ' . #t';\n"
   "}\n"
   "return '#t';\n"
   "} else if (value === false) {\n"
   "if (secondElementOfCons) {\n"
   "return ' . #f';\n"
   "}\n"
   "return '#f';\n"
   "} else if (typeof value === 'number') {\n"
   "if (secondElementOfCons) {\n"
   "return ' . ' + value;\n"
   "}\n"
   "return '' + value;\n"
   "} else if (typeof value === 'object' && value.type === 'box') {\n"
   "if (outermostElement) {\n"
   "return '\\'#&' + correctPrint(value.value, false, false);\n"
   "}\n"
   "return '#&' + correctPrint(value.value, false, false);\n"
   "} else if (typeof value === 'object' && value.type === 'string') {\n"
   "final_string = String.fromCharCode(34);\n"
   "for (index in value.value) {\n"
   "final_string = final_string + value.value[index].value;\n"
   "}\n"
   "final_string = final_string + String.fromCharCode(34);\n"
   "return final_string;\n"
   "} else if (typeof value === 'object' && value.type === 'cons') {\n"
   "if (outermostElement) {\n"
   "return '\\'(' + correctPrint(value.value[0], false, false) + correctPrint(value.value[1], false, true) + ')';\n"
   "} else {\n"
   "if (secondElementOfCons) {\n"
   "if (typeof value === 'object' && value.type === 'cons') {\n"
   "return ' ' + correctPrint(value.value[0], false, false) + correctPrint(value.value[1], false, true);\n"
   "}\n"
   "} else {\n"
   "return '(' + correctPrint(value.value[0], false, false) + correctPrint(value.value[1], false, true) + ')';\n"
   "}\n"
   "}\n"
   "} else if (typeof value === 'object' && value.type === 'vector') {\n"
   "let final_string = '';\n"
   "\n"
   "if (outermostElement) {\n"
   "final_string = final_string + '\\'';\n"
   "}\n"
   "\n"
   "final_string = final_string + '#(';\n"
   "\n"
   "for (let i = 0; i < value.value.length - 1; i ++) {\n"
   "final_string = final_string + correctPrint(value.value[i], false, false) + ' ';\n"
   "}\n"
   "\n"
   "if (value.value.length > 0) {\n"
   "final_string = final_string + correctPrint(value.value[value.value.length - 1]);\n"
   "}\n"
   "final_string = final_string + ')';\n"
   "\n"
   "return final_string;\n"
   "} else {\n"
   "return value;\n"
   "}\n"
   "}\n"))

(define prequel
  (string-append node-imports
                 js-readline
                 higher-order-type-checking
                 make-vector-function
                 make-string-function
                 vector?-function
                 format-output-function))

(define file-conclusion
  (string-append
   "entry().then((returnvalue) => {\n"
   "\treturnvalue == undefined ? '' : console.log(correctPrint(returnvalue, true, false));\n"
   "})\n"))

(define (valid-keyword fun)
  (match (member fun reserved-funs)
    [#f #t]
    [_ #f]))

(define (valid-first-char fun)
  (let ([l (car (string->list fun))])

    (or (char-alphabetic? l) (eq? #\$ l) (eq? #\_ l))))

(define (valid-last-char fun)
  (let ([l (car (reverse (string->list fun)))])
    (or (char-alphabetic? l) (char-numeric? l) (eq? #\_ l))))


(define (valid-fun sym-fun)
  (let ([fun (symbol->string sym-fun)])
    (and (valid-keyword fun) (valid-first-char fun) (valid-last-char fun))))