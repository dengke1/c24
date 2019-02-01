#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "lab6.ss")


(define-test-suite lab6-test-suite

  (test-case 
   "my-reverse-1"
   (check-equal? (my-reverse-1 '(1 2 3)) '(3 2 1))
   (check-equal? (my-reverse-1 '(1 (2 4) 3)) '(3 (2 4) 1)))

  (test-case 
   "num-els-2"
   (check-equal? (num-els-2 '(1 (2 (3) (4 5) (((6))) 7))) 7)
   )


  (test-case
   "flatten-1"
   (check-equal? (flatten-1 '(1 (2 (3) (4 5) (((6))) 7))) '(1 2 3 4 5 6 7))
   )
 )
(display (run-tests lab6-test-suite))
