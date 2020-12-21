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

(define (def? expr) (and (pair? expr) (eq? (car expr) 'define)))
(define (command? expr) (and (pair? expr) (eq? (car expr) 'command)))

(define (repl)
    (let ((machine (calculus)))
        (define (dispatch expr)
            (cond ((def? expr)
                    (bind-val machine (cadr expr) (caddr expr)))
                  ((command? expr)
                    (let ((cmd (cadr expr)))
                        (cond ((eq? cmd 'show) (show-bindings machine))
                              (else 'unknown-command))))
                  (else (eval-lambda machine expr))))
        (define (loop)
            (begin
                (display "> ")
                (define a (read))
                (displayln (dispatch a))
                (loop)))
        (loop)))

(repl)
