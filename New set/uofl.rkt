#lang racket/base
(require racket/port)
(require racket/string)
(require racket/format)


;;; consts

(define version "1.0")

;; better displayln/string-append
(define echo
  (λ args
    (displayln (apply ~a args))))

;; generic about message
(define (about)
  (echo "Calc" version " - interactive calculator\n"
        "Type 'exit' or press ctrl-c to quit"))

;; define a global namespace to allow input-loop eval to understand our commands
(define-namespace-anchor a)
(define input-loop-ns (namespace-anchor->namespace a))

;; double string-replace
(define (string-replace2 s p1 p2 r1 r2)
  (string-replace (string-replace s p1 r1) p2 r2))

;; clean up entry from line-feeds and carriage returns
(define (clean-up command)
  (string-trim (string-replace2 command "\n" "\r" "" "")))

;; display a nice prompt with the current directory
(define (display-prompt)
  (display "UofL>"))

;; import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR FNCT BOOL))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG false true))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))

  (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))


(define (string->boolean x) (if (string=? x "#t") #t #f))  

(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["#f" 'false]
   ["#t" 'true]
   ["sin" (token-FNCT sin)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:or "#t" "#f") (string->boolean lexeme)]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define parser-errors
  (hash
   '((0 . #f))
   "missing lhs of eq"
   '((18 . #f) (6 . x) (0 . #f))
   "missing rhs of eq"
   '((12 . #f) (3 . 1) (0 . #f))
   "missing rhs of plus"
   '((3 . 1) (0 . #f))
   "missing left parenthesis"
   '((20 . 1) (8 . #f) (0 . #f))
   "missing right parenthesis"))

(define calcp
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error
    (lambda (tok-ok? tok-name tok-value #:stack se)
      (define pe
        (hash-ref parser-errors se #f))
      (if pe (error 'calc pe)
          (error
           'calc
           "Unexpected token: ~a~a\nparser-state = ~v"
           tok-name
           (if tok-value
               (format "(~a)" tok-value)
               "")
           se))))

   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))

   (grammar

    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])

    (exp [(NUM) $1]
         [(VAR) (hash-ref vars $1 (lambda () 0))]
         [(VAR = exp) (begin (hash-set! vars $1 $3)
                             $3)]
         [(FNCT OP exp CP) ($1 $3)]
         [(exp + exp) (+ $1 $3)]
         [(exp - exp) (- $1 $3)]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(exp ^ exp) (expt $1 $3)]
         [(OP exp CP) $2]))))

;; run the calculator on the given input-port
(define (calc ip)
  (port-count-lines! ip)
  (let one-line ()
    (define result
      (calcp (lambda () (calcl ip))))
    (when result
      (printf "~a\n" result)
      (one-line))))

(module+ test
  (require rackunit
           racket/port)
  (define (run s)
    (with-output-to-string
      (λ ()
        (calc (open-input-string s)))))
  (define (ok s o)
    (check-equal? (run s) o))
  (define (no s xm)
    (with-handlers
        ([exn:fail?
          (λ (x)
            (check-regexp-match
             xm (exn-message x)))])
      (define o (run s))
      (check-true
       #f
       (format "expected error, got: ~v"
               o))))

  (ok "x=1\n(x + 2 * 3) - (1+2)*3"
      "1\n-2\n")
  (no "(x"
      "right parenthesis")
  (no "x)"
      "left parenthesis")
  (no "x+"
      "missing rhs of plus")
  (no "x="
      "missing rhs of eq")
  (no "=1"
      "missing lhs of eq"))

;; evaluates a string with the calculator
(define (run s)
  (display
   (with-output-to-string
     (λ ()
       (with-handlers
           ([exn:fail?
             (λ (x)
               (displayln (exn-message x)))])
         (calc (open-input-string s)))))))


;-------------------------------inputs------------------------------------------
;initialises variables according to their type
(define (variDef x)
  (if (string=? (substring x (- (string-length x) 1)  (string-length x)) "r")
     (run (string-append (substring x 12 (- (string-length x) 8)) " = 0"))
     (run (string-append (substring x 12 (- (string-length x) 6)) " = 0.0"))
     )
  )

;to apply the keyword input takes input from user and  assigns to variables
(define (inputFromUser x)
  (let ((y (read-line)))
    (begin
      y
      (let ((z (string-append (substring x 6 (string-length x)) " = " y)))
       z)))
  )

;----------------------------------define func----------------------------------

(define (lookup key alist)
  (cond ((null? alist) #f)
        ((eq? key (caar alist)) (cdar alist))
        (else (lookup key (cdr alist)))))

(define (insert key value alist)
  (cons (cons key value) alist))

(define (remove key alist)
  (cond ((null? alist) '())
        ((eq? key (caar alist)) (cdr alist))
        (else (cons (car alist) (remove key (cdr alist))))))

(define dict '())

(define setDictionary
  (lambda (x y)
    (set! dict (insert x y dict))
    ))

(define findDictionary
  (lambda (x)
    (let ((ls '()))
        (set! ls (append ls (lookup x dict)))
      ls)
       ; (display (symbol->string (lookup x dict))))
    ))

(define removeDictionary
  (lambda (x)
    (set! dict (remove 'myInt dict))
    ))

(define (makeFunc x lis)
  (let ((y (read-line)))
    (begin
      (if (string=? y "#definefunc")
       (setDictionary (string->symbol x) lis)
       (set! lis (append lis (list y)))
      ))))

(define (makelist x lis i word ln)
  (begin
   (if (< (- ln 1) i)
        lis
        (begin
        (if (string=? (substring x 0 1) " ")
           (begin
             (set! lis (append lis (list word)))
             (set! word "")
             )
           (begin
             (set! word (string-append word (substring x 0 1)))
           ))
           (makelist (substring x 1 (string-length x)) lis (+ i 1) word ln)
           )
          )
          )
  )
; creates a list of all actions that needs to be done by a function
(define (makefunc x lis)
  (let ((y (read-line)))
    (if(string-prefix? y "#definefunc")
       (setDictionary x lis); stores list of sctions in x or function name
       (begin 
          (set! lis (append lis (list y)))
           (set! y "")
          (makefunc x lis)
          )
    )
  ))
;executes actions in a functon by extracting them from the list
(define (workfunc lis)
  (if (null? lis)
      lis
      (begin
        (cond
          ((string-prefix? (car lis) "input") (run (inputFromUser (car lis))))
          ((string-prefix? (car lis) "output") (run (substring (car lis) 7 (string-length (car lis)))))
          (else (run (car lis))))
        (workfunc (cdr lis))
        )
      )
  )

(define (defineFuncControl x)
  (let ((lis '()))
    (set! x (string-append x " "))
   (set! lis (append lis (makelist x '() 0 "" (string-length x))))
   (makefunc (string->symbol (car lis)) (cdr lis)); creates a list of actions and stores in func name
   ;(workfunc (findDictionary (string->symbol (car lis))))
))

(define (funcCall list1 list2 listfin)
  (if (null? list1)
      (append listfin list2)
      (begin
        (set! listfin (append listfin (list (string-append (car list2) "=" (car list1)))))
        (funcCall (cdr list1) (cdr list2) listfin)
        )
      ))

(define (funcRun x)
    (let ((lis '()) (lis2 '()))
    (set! x (string-append x " "))
    (set! lis (append lis (makelist x '() 0 "" (string-length x)))); makes list '("myFunc" "1" "2")
    (set! lis2 (append lis2 (findDictionary (string->symbol (car lis)))))
    (workfunc (funcCall (cdr lis) lis2 '()))
      ))
;(makefunc 'myInt '("a" "b"))
;(define z 'myInt)
;(workfunc (findDictionary z))
;(workfunc '("a" "b" "c"))

;------------------------------------controls------------------------------------
;; input loop
(define (input-loop)
  (let ((flag 0))
  (let/ec break
    (let loop ()
      ; display command prompt
      (display-prompt)                                                                         
      ; get input from user; trim and remove annoying enters and returns
      (define command
        (clean-up (read-line)))
      (cond [(string=? command "") (loop)]
            [(string-prefix? command "#definevari") (begin (display "defining variable") (variDef command) (newline))]; for defining variables
            [(string-prefix? command "#definefunc") (begin (defineFuncControl (substring command 12 (string-length command))) (newline))]; for define function
            [(string-prefix? command "input") (begin (run (inputFromUser command)))];foe keyword input
            [(string-prefix? command "output") (begin (run (substring command 7 (string-length command))))]; to print the result, need to remove print from run command
            [(string-prefix? command "#clear") (begin (display "call to clear") (set! flag 1) (break) (input-loop) (newline))]; to clear
            [(string-prefix? command "#exit") (set! flag 0) (break)]; to exit
            [else
             (begin
               (if (findDictionary (string->symbol (car (makelist (string-append command " ") '() 0 "" (string-length (string-append command " "))))))
                 (funcRun command)
                 (run command)
               ))]); to do any mathematical operation
      (loop)))
  flag))

;;; main
;(run "a =20")
;(run "#definevari a")
(define main
  (begin
   (about)
   (newline)
   (if (eq? (input-loop) 1)
       (input-loop)
      ; (main)
       (display "Exited properly from UofL"))
   "in main")
)

; EOF
;------------------------------------------------------------------------------------------


;-------------------------------------------------------------------------------------------
;(define start
 ;(if (string=? ">(uofl)" (read-line))
     ; (main) 
  ;    ((display "Type >(uofl)to start inetrpreter")
   ;    (start))))

;(start)

