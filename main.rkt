#lang racket

(require "lambda.rkt")
(require compatibility/mlist)

(define (calculus)
    (let ((bindings (list->mlist null)))
        (define (add-binding bnd val)
          (set! bindings (mcons (list bnd val) bindings)))
        (define (dispatch message)
            (cond ((eq? message 'bind)
                    (lambda (bnd val) 
                        (if (mmember bnd (mmap car bindings))
                            'multiple-binding
                            (begin
                                (add-binding bnd val)
                                'done))))
                  ((eq? message 'eval)
                    (lambda (expr)
                        (let ((result (filter (lambda (x) (not (member (car x) (find-vars expr) ))) (mlist->list bindings))))
                            (evaluate 
                                (replace-vars expr (map car result) (map cadr result))))))
                  ((eq? message 'show) bindings)
                  (else (error "Unknown"))
            ))
        dispatch))

(define (bind-val machine bnd val) ((machine 'bind) bnd val))
(define (eval-lambda machine expr) ((machine 'eval) expr))
(define (show-bindings machine) (machine 'show))

(define c (calculus))

(begin
    (displayln (bind-val c 'a 2))
    (displayln (bind-val c 'b 3))
    (displayln (bind-val c 'b 3))
    (displayln (show-bindings c))
    (displayln (eval-lambda c '((lambda (x) (x b)) c)))
)
