#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [`(add1 ,x) (expr? x)]
    [`(sub1 ,x) (expr? x)]
    [`(abs ,x) (expr? x)]
    [`(square ,x) (expr? x)]
    [`(cond ,(list a b) ...) 
      (and (andmap cond_stmt? a) (andmap expr? b))
    ]
    [`(if (zero? ,e0) ,e1, e2)
     (and (expr? e0)
          (expr? e1)
          (expr? e2))]
    [_ (display x)(printf " failure in expr\n")#f]))

(define (cond_stmt? x)
  (match x
    [`(zero? ,e0) (expr? e0)]
    [else #t]
  )
)