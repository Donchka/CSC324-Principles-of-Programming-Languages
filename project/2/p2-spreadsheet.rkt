#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt")
;(require "p2-soln.rkt")

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide type-check-spreadsheet fill-in)

;-------------------------------------------------------------------------------
; * Task 3: Type Checking a Spreadsheet *
;-------------------------------------------------------------------------------

#|
(type-check-spreadsheet spreadsheet)
  spreadsheet: An spreadsheet AST described in the project handout.

  Returns a list of booleans, representing whether the types of each column
  is correctly annotated.
|#
(define (type-check-spreadsheet spreadsheet)
  (let* ([defs (rest (second spreadsheet))]
         [columns (rest (third spreadsheet))]
         [def-env (foldl
                   (lambda (def env) (cons (cons (first def) (let* ([type-check (run 1 (out) (typeo (second def) env out))])
                                                               (cond
                                                                 [(empty? type-check) #f]
                                                                 [else (first type-check)]))) env))
                   '()
                   defs)];build the type environment for definitions(include typechecking)
         [col-required-type (foldl
                             (lambda (col acc) (cons (cons (first col) (second col)) acc))
                             '()
                             columns)]
         [result (foldl (lambda (col env) (let* ([column-type-anno (second col)];required type for type checking
                                                 [column-name (first col)]
                                                 [data (rest (third col))])
                                            (append (list (cons column-name (if (foldl
                                                                                 (lambda (value consistent) (let* ([type-check (run 1 (out) (typeo value env out))])
                                                                                                              (cond
                                                                                                                [(empty? type-check) #f]
                                                                                                                [(not (equal? (first type-check) column-type-anno)) #f]
                                                                                                                [else consistent])))
                                                                                 #t
                                                                                 data) column-type-anno #f))) env)))
                        def-env
                        columns)]
         )
    (map (lambda (col) (let* ([column-name (first col)]
                              [required-type (lookup column-name col-required-type)]
                              [checked-type (lookup column-name result)])
                         (equal? required-type checked-type)))
         columns)))


(define/match (lookup key alst)
  [(key '()) #f]
  [(key (cons x xs)) (if (equal? (car x) key) (cdr x) (lookup key xs))])
;-------------------------------------------------------------------------------
; * Task 4: Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the spreadsheet grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the spreadsheet expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (run n (lvar) (typeo expr '() type))
    ]))


