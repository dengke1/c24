#lang scheme

(require "lab3.ss")
(require test-engine/scheme-tests)

(check-expect (my-length empty) 0)

(check-expect (my-length '(1 2 3)) 3)


(check-expect (my-reverse '(1 2 3)) '(3 2 1))

(check-expect (my-reverse '()) '())

(check-expect (my-reverse '(3)) '(3))


(check-expect (is-pal '(1 2 3)) #f)

(check-expect (is-pal '(1 2 3 2 1)) #t)

(check-expect (is-pal '()) #t)


(check-expect (num-el '(1 2 3)) 3)

(check-expect (num-el '(1 ((3) 2) 3 (1 2))) 6)

(check-expect (num-el '(1)) 1)

(check-expect (num-el '()) 0)

  
(check-expect (stutter '()) '())

(check-expect (stutter '(1 2)) '(1 1 2 2))

(check-expect (stutter '(2 4 5)) '(2 2 4 4 5 5))


(define hello(λ (c)(if (> c 2) #t #f)))

(check-expect (my-filter hello '(1 2)) '())

(check-expect (my-filter hello '(1 2 3 4 5)) '(3 4 5))

(check-expect (my-filter hello '()) '())


(check-expect (my-map hello '(1 2)) '(#f #f))

(check-expect (my-map hello '(1 2 3 4 5)) '(#f #f #t #t #t))

(check-expect (my-map hello '()) '())


(test)