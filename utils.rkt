#lang racket
(provide (all-defined-out))
(define (format-str str . xs)
    (match xs
    ['() (if (string-contains? str "%s") (error "Toom many format specifiers!") str)]
    [(cons x rest) (apply format-str (string-replace str "%s" x #:all? #false) rest)]
    )
)