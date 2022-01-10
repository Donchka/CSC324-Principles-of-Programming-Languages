#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#
(define (typeof expr typeenv)
  (cond
    ; Constants
    [(number? expr) 'num]
    [(string? expr) 'str]
    [(boolean? expr) 'bool]

    ; Identifiers
    [(symbol? expr) (lookup expr typeenv)]

    ; Builtins
    [(lookup (first expr) '((+ . #t)
                            (- . #t)
                            (* . #t)
                            (/ . #t))) (if
                                        (and (equal? (typeof (second expr) typeenv) 'num) (equal? (typeof (third expr) typeenv) 'num))
                                        'num
                                        'error)]
    [(lookup (first expr) '((> . #t)
                            (= . #t)
                            (>= . #t))) (if
                                        (and (equal? (typeof (second expr) typeenv) 'num) (equal? (typeof (third expr) typeenv) 'num))
                                        'bool
                                        'error)]
    [(equal? (first expr) '++) (if
                                (and (equal? (typeof (second expr) typeenv) 'str) (equal? (typeof (third expr) typeenv) 'str))
                                'str
                                'error)]
    [(equal? (first expr) '!) (if
                               (equal? (typeof (second expr) typeenv) 'bool)
                               'bool
                               'error)]
    [(equal? (first expr) 'num->str) (if
                                      (equal? (typeof (second expr) typeenv) 'num)
                                      'str
                                      'error)]
    [(equal? (first expr) 'len) (if
                                 (equal? (typeof (second expr) typeenv) 'str)
                                 'num
                                 'error)]

    ; Function Calls
    [else (if
           (equal? (map (lambda (arg) (typeof arg typeenv)) (rest expr)) (first (lookup (first expr) typeenv)))
           (second (lookup (first expr) typeenv))
           'error)]
  ))

; Helper functions for Task 1

#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define/match (lookup key alst)
  [(key '()) #f]
  [(key (cons x xs)) (if (equal? (car x) key) (cdr x) (lookup key xs))])

; Add your helper functions here
(define (boolo obj)
  (conde ((== obj #t)) ((== obj #f))))


;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
   ; constants: numbero, stringo, and boolo are miniKanren builtin relations
   ((numbero expr)
    (== type 'num))
   ((stringo expr)
    (== type 'str))
   ((boolo expr)
    (== type 'bool))

   ; identifier: symbolo is a miniKanren builtin relation
   ((symbolo expr)
    (lookupo expr env type))

   ; builtins
   ((fresh (operator numbers subexpr1 subexpr2)
           (== expr (cons operator numbers))
           (== numbers (cons subexpr1 (list subexpr2)))
           (conde
            ((== operator '+))
            ((== operator '-))
            ((== operator '*))
            ((== operator '/)))
           (typeo subexpr1 env 'num)
           (typeo subexpr2 env 'num)
           (== type 'num)))

   ((fresh (operator numbers subexpr1 subexpr2)
           (== expr (cons operator numbers))
           (== numbers (cons subexpr1 (list subexpr2)))
           (conde
            ((== operator '>))
            ((== operator '=))
            ((== operator '>=)))
           (typeo subexpr1 env 'num)
           (typeo subexpr2 env 'num)
           (== type 'bool)))

   ((fresh (operator strings subexpr1 subexpr2)
           (== expr (cons operator strings))
           (== strings (cons subexpr1 (list subexpr2)))
           (== operator '++)
           (typeo subexpr1 env 'str)
           (typeo subexpr2 env 'str)
           (== type 'str)))

   ((fresh (operator arg)
           (== expr (list operator arg))
           (== operator '!)
           (typeo arg env 'bool)
           (== type 'bool)))

   ((fresh (operator arg)
           (== expr (list operator arg))
           (== operator 'num->str)
           (typeo arg env 'num)
           (== type 'str)))

   ((fresh (operator arg)
           (== expr (list operator arg))
           (== operator 'len)
           (typeo arg env 'str)
           (== type 'num)))

   ; function calls
   ((fresh (fcn args types fcntype)
           (== expr (cons fcn args))
           (typeo fcn env fcntype)
           (== fcntype (list types type))
           (type-listo args env types)))
            

   ; function definitions
   ((fresh (name def-arg body argtypes new-env return-type)
           (== expr (list name def-arg body))
           (def-inito def-arg env new-env)
           (typeo body new-env return-type)
           (type-listo def-arg new-env argtypes)
           (== type (list argtypes return-type))))

    
   ))


; Helper functions for Task 2

#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#
(define (lookupo key lst value)
  (fresh (fkey fval rest)
         (== (cons (cons fkey fval) rest) lst)
         (conde ((== key fkey)
                 (== value fval))
                ((=/= key fkey)
                 (lookupo key rest value)))))

#|
(type-listo exprs env types)
  exprs: A list of expression 
  env: An association list
  types: The corresponding types of exprs
|#
(define (type-listo exprs env types)
  (conde
   ((== '() exprs)
    (== '() types))
   ((fresh (expr rest-exprs type rest-types)
         (== exprs (cons expr rest-exprs))
         (== types (cons type rest-types))
         (typeo expr env type)
         (type-listo rest-exprs env rest-types)))))


(define appendo
  (lambda (l s out)
    (conde
     [(== '() l) (== s out)]
     [(fresh (a d res)
             (== `(,a . ,d) l)
             (== `(,a . ,res) out)
             (appendo d s res))])))


#|
(def-inito def-args env new-env)
  def-args: A list of arguments(identifiers) in the function definition 
  env: An association list
  new-env: A new association list after adding all ids in def-args

This relation holds when (argument . type) pairs are added to
the env for each argument in def-args. The type is not constrained
here, so it is considered as initial value.
|#
(define (def-inito def-args env new-env)
  (conde
   ((== '() def-args)
    (== env new-env))
   ((fresh (fst rest type new)
           (== def-args (cons fst rest))
           (appendo (list (cons fst type)) env new)
           (def-inito rest new new-env)))))
           
