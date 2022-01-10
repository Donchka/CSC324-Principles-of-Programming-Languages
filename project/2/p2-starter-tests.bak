#lang racket #| CSC324 Fall 2020: Project 2 Starter Tests|#

(require "p2-base.rkt")
(require "p2-spreadsheet.rkt")
(require "mk.rkt")

(require rackunit)

(define all-tests
  (test-suite
   "all-tests"
   (test-suite
    "task1"
    (test-equal? "(typeof 3 '())"  ; Test label
                 (typeof 3 '())    ; Actual value
                 'num)             ; Expected value

    (test-equal? "(typeof 'hello' '())"
                 (typeof "hello" '())
                 'str)

    (test-equal? "(typeof #t '())"
                 (typeof #t '())
                 'bool)

    (test-equal? "(typeof (= 3 3) '())"
                 (typeof (= 3 3) '())
                 'bool)

    (test-equal? "(typeof a '((a . num) (b . str)))"
                 (typeof 'a '((a . num) (b . str)))
                 'num)

    (test-equal? "(typeof '(+ a a) '((a . num) (b . str)))"
                 (typeof '(+ a a) '((a . num) (b . str)))
                 'num)

    (test-equal? "(typeof '(+ 3 'hello') '())"
                 (typeof '(+ 3 "hello") '())
                 'error)

    (test-equal? "(typeof 'f '((f . ((num) num)) (g . ((num) str))))"  
                 (typeof 'f '((f . ((num) num)) (g . ((num) str))))    
                 '((num) num))

    (test-equal? "(typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) num))))"
                 (typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) num))))
                 'num)

    (test-equal? "(typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) str))))"
                 (typeof '(f (g 3)) '((f . ((num) num)) (g . ((num) str))))
                 'error))

   (test-suite
    "task2"
    (test-equal? "(run 1 (out) (typeo '(> 4 5) '() out))"
                 (run 1 (out) (typeo '(> 4 5) '() out))
                 '(bool))

    (test-equal? "(run 1 (out) (typeo '(> 4 'hi') '() out))"
                 (run 1 (out) (typeo '(> 4 "hi") '() out))
                 '())

    (test-equal? "(run 1 (out) (typeo '((lambda (x) x) 3) '() out))"
                 (run 1 (out) (typeo '((lambda (x) x) 3) '() out))
                 '(num)))

    (test-equal? "(run 1 (out) (typeo '((lambda (x) (g x))) '((g . ((num) num))) out))"
                 (run 1 (out) (typeo '(lambda (x) (g x)) '((g . ((num) num))) out))
                 '(((num) num)))


   (test-suite
    "task3"
    (test-equal? "sample in project handout"
                    (type-check-spreadsheet
                       '(spreadsheet
                           (def (voting-age 18)
                                (canvote (lambda (x) (>= x voting-age))))
                           (columns
                             (name  str (values "adam" "betty" "clare" "eric" "sam"))
                             (age   num (values 12 15 18 49 17))
                             (voter bool (computed (canvote age)))
                             (voter2 bool (computed (>= age name))))))
                     '(#t #t #t #f)))
   (test-suite
    "task4"
    (test-equal? "fill-in returns some answers"
                 (length (fill-in BLANK `(+ 3 ,BLANK) 'num 2))
                 2))

  ))

;-------------------------------------------------------------------------------

; Run and display tests
(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
