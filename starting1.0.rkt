#lang racket
(define (interpreter)
  (display "uofl> ")
  (let ((input (read)))
    
    (let ((result (eval input)))
      (display result)
      (newline))))

;type (intVal a' 10)
(define (intVal varName val)
  (define intval val)
  (let ((newVarName intval))
    newVarName))

(define (convert-expression expr)
  (if (and (list? expr) (= (length expr) 3) (eq? (cadr expr) '+))
      `(+ ,(car expr) ,(caddr expr))
      "Invalid expression"))

; Example usage:
;(convert-expression '(a + b)) ; Returns (+ a b)