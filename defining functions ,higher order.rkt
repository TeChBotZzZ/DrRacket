#lang racket
(define (add x y)
  (+ x y))
(define (map-fn fn 1st)
  (if (null? 1st)
      '()
      (cons (fn (car 1st))
            (map-fn fn(cdr 1st)))))

(define (square x)
  (* x x))


(define add-one (lambda (x) (+ x 1)))

(add-one 5)  ; returns 6

;defining a int
(define integer?
  (lambda (x)
    (and (number? x) (integer? x))))

(define float?
  (lambda (x)
    (and (number? x) (not (integer? x)))))



(define boolean?
  (lambda (x)
    (or (eq? x #t) (eq? x #f))))

; sample calculator
(define (calculator exp)
  (cond ((null? exp) 0)                 ; zero terms
        ((null? (cdr exp)) (car exp))   ; one  term only (assumed number)
        (else                           ; assumed three or more terms
          (let ((operand1 (car  exp))
                (operator (cadr exp))
                (operands (cddr exp)))
            ((case operator             ; convert operator, a symbol, to its function
               ((+) +)
               ((-) -))
             operand1
             (calculator operands)))))) ; calculate the rest
> (calculator '(1 + 2))
3
> (calculator '(1 - 2 + 3))
-4






(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
        (else (error "Calc: bad expression:" exp))))

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
        ((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
                           ((= (length args) 1) (- (car args)))
                           (else (- (car args) (accumulate + 0 (cdr args))))))
        ((eq? fn '*) (accumulate * 1 args))
        ((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
                           ((= (length args) 1) (/ (car args)))
                           (else (/ (car args) (accumulate * 1 (cdr args))))))
        (else (error "Calc: bad operator:" fn))))
