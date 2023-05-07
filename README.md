# rackjs

Basic Racket to JavaScript compiler.

Usage:

    - To run tests: `racket test/test.rkt`
    - If you have Racket code in a file called `sample.rkt` and want
    to compile to JS, run `make sample.js` (and then to run: `node sample.js`)

Notes about behavior:

    - type-checking is run time. If you type (add1 #f), it will compile, but throw an error when running
