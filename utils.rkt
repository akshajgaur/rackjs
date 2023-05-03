#lang racket
(provide (all-defined-out))

(define (format-str str . xs)
    (match xs
    ['() (if (string-contains? str "%s") (error "Toom many format specifiers!") str)]
    [(cons x rest) (apply format-str (string-replace str "%s" x #:all? #false) rest)]))


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
(define file-conclusion (string-append  "entry().then((returnvalue) => {\n"
                                            "\treturnvalue == undefined ? '' : console.log(returnvalue);\n"
                                        "})\n"
))
