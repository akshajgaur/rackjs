#lang racket
(provide (all-defined-out))

(define (format-str str . xs)
    (match xs
    ['() (if (string-contains? str "%s") (error "Toom many format specifiers!") str)]
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

(define prequel (string-append node-imports js-readline higher-order-type-checking make-vector-function vector?-function))

(define file-conclusion (string-append  "entry().then((returnvalue) => {\n"
                                            "\treturnvalue == undefined ? '' : console.log(returnvalue);\n"
                                        "})\n"
))
