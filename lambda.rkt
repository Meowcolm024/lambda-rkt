#lang racket

(provide evaluate find-vars replace-vars)

;; lambda calculus
;; <exp> ::= <var>
;;        |  (<exp> <exp>)
;;        |  (λ (<var>) <exp>)

(define (evaluate expr)
    (let ((result (eval-expr expr)))
        (if (equal? result expr)
            result
            (evaluate result))))

(define (eval-expr expr)
    (cond ((lambda? expr) (handle-lambda expr))
          ((pair? expr) (handle-term expr))
          (else expr)))

(define (handle-lambda expr) 
   (let ((vars (cadr expr))
        (body (caddr expr)))
        (list 'lambda vars (eval-expr body))))

(define (handle-term expr) 
    (if (lambda? (car expr))
        (let ((lam (eval-expr (car expr)))
              (body (eval-expr (cadr expr))))
             (let ((left-vars (find-vars lam))
                   (right-vars (find-vars body)))
                  (replace (caddr lam) (car (cadr lam)) 
                    (replace-vars body right-vars (handle-dup left-vars (find-vars body))))))
        (let ((result (map eval-expr expr)))
            (if (pair? result)
                (if (lambda? (car result))
                    (eval-expr result)
                    result)
                result))))

;; handle beta
(define (find-vars expr)
    (cond ((lambda? expr) (cons (car (cadr expr)) (find-vars (caddr expr))))
          ((pair? expr) (append (find-vars (car expr)) (find-vars (cadr expr))))
          (else null)))

(define (replace expr var val)
    (if (pair? expr)
        (map (lambda (x)
                (if (pair? x)
                    (replace x var val)
                    (if (eq? x var) val x))) 
            expr)
        (if (eq? expr var) val expr)))

(define (replace-vars expr vars vals)
    (if (null? vars)
        expr
        (replace-vars 
            (replace expr (car vars) (car vals))
            (cdr vars)
            (cdr vals))))

;; helper functions
(define (lambda? expr)
    (if (pair? expr)
        (eq? (car expr) 'lambda)
        #f))

(define (intersection a b)
  (if (null? a)
      '()
      (if (member (car a) b)
          (cons (car a) (intersection (cdr a) b))
          (intersection (cdr a) b))))

(define (quote-append org next)
    (string->symbol 
        (string-append (symbol->string org) (symbol->string next))))

(define (handle-dup list-a list-b)
    (let ((common (intersection list-a list-b)))
            (if (null? common)
                list-b
                (handle-dup 
                    list-a 
                    (map (lambda (x) 
                            (if (member x common) (quote-append x '_) x))
                         list-b)))))
