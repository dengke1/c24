#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "evaluator.ss")

(define-test-suite evaluator-test-suite

  (test-equal? 
   "evaluate1"
   (evaluate '(a and b)
             '((a . #t) (b . #f) (c . #t)))
   #f)
  
  (test-equal? 
   "evaluate1"
   (evaluate '(a and (b or c))
             '((a . #t) (b . #f) (c . #t)))
   #t)

  (test-equal? 
   "evaluate1"
   (evaluate '(#f and (#t or c))
             '((a . #t) (b . #f) (c . #t)))
   #f)
  
  (test-equal? 
   "evaluate"
   (evaluate '((a and (not b)) or ((a and c) or ((not b) and c)))
             '((a . #t) (b . #f) (c . #f) (d . #t)))
   #t)
  )

(define-test-suite simplifier-test-suite
  
  (test-equal? 
   "simplify"
   (simplify '(a and (b or c))
             '((a . #t) (b . #f) (c . #t)))
   #t)
  
  (test-equal? 
   "simplify"
   (simplify '(b or c)
             '((a . #t) (b . #f) (c . #t)))
   #t)
  
  
  (test-equal? 
   "simplify"
   (simplify '((a and b) and (b or c))
             '((b . #f) (c . #t)))
   #f)
  
  (test-equal? 
   "simplify"
   (simplify '(a and (b or c))
             '((c . #t)))
   'a)

  (test-equal? 
   "simplify"
   (simplify '((a and (not b)) or ((a and c) or ((not b) and c)))
             '((a . #t) (c . #f) (d . #t)))
   '(not b))
  
  (test-equal? 
   "simplify"
   (simplify '((#t and (not b)) or ((a and #f) or ((not b) and #f)))
             '((a . #t) (c . #f) (d . #t)))
   '(not b))
  
  )

(display (run-tests evaluator-test-suite))
(display (run-tests simplifier-test-suite))