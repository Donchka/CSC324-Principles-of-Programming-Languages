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
                 'error)
    
    (test-equal? "(typeof '(f (g 3) (h 1)) '((f . ((num) str)) (g . ((num) num))) (h . ((num num num) str))))"
                 (typeof '(f (g 3) (h 1 2 3)) '((f . ((num str) str)) (g . ((num) num)) (h . ((num num num) str))))
                 'str)

    (test-equal? "function returning a function"
                 (typeof '(g 3) '((g . ((num) ((num num) num)))))
                 '((num num) num)))

   (test-suite
    "task2"
    (test-equal? "(run 1 (out) (typeo '(> 4 5) '() out))"
                 (run 1 (out) (typeo '(> 4 5) '() out))
                 '(bool))

    (test-equal? "(run 1 (out) (typeo '(> 4 'hi') '() out))"
                 (run 1 (out) (typeo '(> 4 "hi") '() out))
                 '())

    (test-equal? "(run 1 (out) (typeo 'a '((a . num)) out))"
                 (run 1 (out) (typeo 'a '((a . num)) out))
                 '(num))

    (test-equal? "(run 1 (out) (typeo 'f '((f . ((num) num)) (g . ((num) str))) out))"
                 (run 1 (out) (typeo 'f '((f . ((num) num)) (g . ((num) str))) out))
                 '(((num) num)))

    (test-equal? "(run 1 (out) (typeo 'f '((f . ((num) num)) (g . ((num) str))) out))"
                 (run 1 (out) (typeo '(f 3) '((f . ((num) num)) (g . ((num) str))) out))
                 '(num))

    (test-equal? "(run 1 (out) (typeo '(g (f 3)) '((f . ((num) num)) (g . ((num) str))) out))"
                 (run 1 (out) (typeo '(g (f 3)) '((f . ((num) num)) (g . ((num) str))) out))
                 '(str))

    (test-equal? "(run 1 (out) (typeo '((lambda (x) x) 3) '() out))"
                 (run 1 (out) (typeo '((lambda (x) x) 3) '() out))
                 '(num)))

   (test-equal? "(run 1 (out) (typeo '((lambda (x y) (+ x y)) 3 4) '() out))"
                 (run 1 (out) (typeo '((lambda (x y) (+ x y)) 3 4) '() out))
                 '(num))

   (test-equal? "(run 1 (out) (typeo '((lambda (x) (+ x y))) '((g . ((num) num)) (h . ((num str) bool))) out))"
              (run 1 (out) (typeo '(lambda (x y z) (h (g x (+ (len (g z 1)) y)) (num->str x))) '((g . ((num num) str)) (h . ((str str) bool))) out))
                 '(((num num num) bool)))

    (test-equal? "(run 1 (out) (typeo '(lambda (x) (g x)) '((g . ((num) num))) out))"
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
                     '(#t #t #t #f))

    (test-equal? "Task 3 EXPR Zoo"
                    (type-check-spreadsheet
                       '(spreadsheet
                           (def (driving-age 16)
                                (driving-age2 driving-age)
                                (voting-age (+ driving-age2 2))
                                (condition-checker (lambda (x y) (>= x y)))
                                (get-fullname (lambda (x y) (++ x y)))
                                (get-namelength (lambda (x) (len x)))
                                (cannotvote (lambda (x) (! x)))
                                (cannotvote2 cannotvote)
                                (age-name (lambda (x y z) (++ (num->str x) (get-fullname y z)))))
                           (columns
                             (first-name  str (values "harry" "betty" "clare" "eric" "sam"))
                             (last-name  str (values "potter" "crocker" "blare" "cire" "bam"))
                             (age   num (values 12 15 18 49 2000))
                             (voter bool (computed ((lambda (x) (condition-checker x voting-age)) age)))
                             (driver bool (computed ((lambda (x) (condition-checker x driving-age2)) age)))
                             (full-names str (computed (get-fullname first-name last-name)))
                             (lengths num (computed (get-namelength full-names)))
                             (notvoter bool (computed (cannotvote2 voter)))
                             (age-and-name str (computed (age-name age first-name last-name)))
                             )))
                     '(#t #t #t #t #t #t #t #t #t))

    (test-case  "Task 3 conflicting column types"
                 (let* ([call (type-check-spreadsheet
                  '(spreadsheet
                    (def (voting-age 2)
                         (is-2 (lambda (x) (= x 2) ))
                      )
                    (columns
                         (id str (values 1 2 3 4 5))
                         (two bool (computed (is-2 id)))
                        )))])
                   ; could be two cases
                   (or (check-equal? call '(#f #f))
                       (check-equal? call '(#f #t)))))

    (test-equal? "Task 3 EXPR Zoo with incorrect type"
                    (type-check-spreadsheet
                       '(spreadsheet
                           (def (driving-age 16)
                                (driving-age2 driving-age)
                                (voting-age (+ driving-age2 2))
                                (condition-checker (lambda (x y) (>= x y)))
                                (get-fullname (lambda (x y) (++ x y)))
                                (get-namelength (lambda (x) (len x)))
                                (cannotvote (lambda (x) (! x)))
                                (cannotvote2 cannotvote)
                                (age-name (lambda (x y z) (++ (num->str x) (get-fullname y z)))))
                           (columns
                             (first-name  str (values "harry" "betty" "clare" "eric" "sam"))
                             (last-name  str (values "potter" 3 "blare" "cire" "bam"))
                             (age   num (values 12 15 18 49 2000))
                             (voter bool (computed ((lambda (x) (condition-checker x voting-age)) age)))
                             (driver bool (computed ((lambda (x) (condition-checker x driving-age2)) age)))
                             (full-names str (computed (get-fullname first-name last-name)))
                             (lengths num (computed (get-namelength full-names)))
                             (notvoter bool (computed (cannotvote2 voter)))
                             (age-and-name str (computed (age-name age first-name last-name))))))
                     '(#t #f #t #t #t #f #f #t #f))

     (test-equal? "sample in project handout"
                    (type-check-spreadsheet
                       '(spreadsheet
                           (def (voting-age "s")
                                (canvote (lambda (x) (>= x voting-age))))
                           (columns
                             (name  str (values "adam" "betty" "clare" "eric" "sam"))
                             (age   num (values 12 15 18 49 17))
                             (voter bool (computed (canvote age)))
                             (voter2 bool (computed (>= age name))))))
                     '(#t #t #f #f))
    )
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
