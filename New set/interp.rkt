#lang racket
(provide (all-defined-out))

;; Expr -> Integer
(define (interp e)
  (match e
    [(? integer? i) i]
    [`(add1 ,e0)
     (+ (interp e0) 1)]
    [`(sub1 ,e0)
     (- (interp e0) 1)]
    [`(if (zero? ,e0) ,e1 ,e2)
     (if (zero? (interp e0))
         (interp e1)
         (interp e2))]
    [`(cond ,(list a b) ...)
     (define b_ind 0)
     (define res '())
     (display a) ; debugging
     (printf "\n")
     (display b) ; debugging 
     (printf "\n")
     (for-each (lambda (a) (
                            (match a
                              [`(zero? ,e0)
                               (if (and (empty? res) (zero? (interp e0)))
                                   (append res (interp (list-ref b b_ind)))
                                   (+ b_ind 1))]
                              [`else
                               (if (empty? res)
                                   (append res (interp (list-ref b b_ind)))
                                   (`())) ])))
               a)
     (first res)
     ]))

