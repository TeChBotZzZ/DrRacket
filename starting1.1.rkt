#lang racket
(define env '()) ; initialize the environment as an empty association list

(define (lookup varName env)
  (cond ((null? env) (error "Unbound variable" varName))
        ((eq? varName (caar env)) (cdar env))
        (else (lookup varName (cdr env)))))

(define (intVal varName val)
  (set! env (cons (cons varName val) env))
  val) ; return the value of the integer variable

(define (interpreter env)
  (display "uofl> ")
  (let ((input (read)))
    (let ((result (eval input env)))
      (display result)
      (newline))
    (interpreter env))) ; repeat the interpreter loop with the updated environment

(interpreter env) ; start the interpreter loop with the empty environment




;just an example for help
; example to store data 
;(define-record-type employee
 ; (make-employee name age salary)
  ;employee?
  ;(name employee-name)
  ;(age employee-age)
  ;(salary employee-salary))

;(define john (make-employee "John" 30 50000))
;(define jane (make-employee "Jane" 25 40000))

; Example usage:
;(employee-name john) ; Returns "John"
;(employee-salary jane) ; Returns 40000