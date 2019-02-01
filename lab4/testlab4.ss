#lang scheme

(require "lab4.ss")
(require test-engine/scheme-tests)

(check-expect (dot-product-rec '(1 -2 6) '(-2 0 1)) 4)
(check-expect (dot-product '(1 -2 6) '(-2 0 1)) 4)

(check-expect (vector-add '(1 2 3) '(-1 3 -2)) '(0 5 1))
(check-expect (vector-add-rec '(1 2 3) '(-1 3 -2)) '(0 5 1))

(define m1 '((1 0 2)
             (2 1 4)
             (-1 1 -1)))
(define m2 '((1 2 3)
             (4 5 6)
             (7 8 9)))
(define m3 '((2 2 5)
             (6 6 10)
             (6 9 8)))
(define m4 '((15 18 21)
             (34 41 48)
             (-4 -5 -6)))
(check-expect (add m1 m2) m3)

(check-expect (scalar-vector-mult 2 '(2 3 4)) '(4 6 8))

(define m5 '((2 0 4)
             (4 2 8)
             (-2 2 -2)))
(check-expect (scalar-mult 2 m1) m5)

(define m1tr '((1 2 -1)
               (0 1 1)
               (2 4 -1)))
(check-expect (transpose m1) m1tr)

(check-expect (mult m1 m2) m4)

(test)
