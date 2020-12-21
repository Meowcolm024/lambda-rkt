#lang racket

(require "lambda.rkt")

;; tests
(begin
    ;; and false true
    (displayln (evaluate
        '(
            (
                (lambda (p) (lambda (q) ((p q) p)))
                (lambda (x) (lambda (y) x))
            ) 
            (lambda (x) (lambda (y) y))
        )
    ))
    ;; succ zero
    (displayln (evaluate
        '(
            (lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))))
            (lambda (f) (lambda (x) x))
        )
    ))
)
