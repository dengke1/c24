#lang scheme

; (1) Begin by evaluating some simple expressions:

;  1
; '1
; 'x
; x
; '(1 2 3)
; (1 2 3)
; (+ 1 2)
; (* (+ 1 2) 2)

; to get into the habit of prefix notation.

; Now evaluate a few more yourself.


; (2) Now we make some definitions:

; make x be 42
(define x 42)

; make y be x + 1
(define y (+ x 1))

; make z be x - y
(define z (- x y))

; Now evaluate x, y, and z.


; (3) And finally, functions!

; Evaluate:

; a function with parameter x that returns x + 1
(λ (x) (+ x 1))
; result of applying a function with parameter x that returns x + 1, to 42
((λ (x) (+ x 1)) 42)

; make inc be a function with parameter x that returns x + 1
(define inc
  (λ (x) (+ x 1)))

; symbol inc
inc

; result of applying a function, obtained by evaluating symbol inc, to 10
(inc 10)

; a function with parameters x and y that returns x + y + 1
(λ (x y) (+ x y 1))

; make strange-add be
; a function with parameters x and y that returns x + y + 1
(define strange-add
  (λ (x y) (+ x y 1)))

; result of applying a function, obtained by evaluating symbol strange-add,
; to 42 and 10
(strange-add 42 10)


; Your turn!

; Define a function named plus42 that takes one argument
; and returns the sum of the argument and 42.
(define plus42
  (λ (x) (+ x 42)))

; Define a function called my-avg that takes two arguments and returns
; their average (assume arguments are numbers).
(define my-avg
  (λ (x y) (/(+ x y) 2)))
; Define a function called apply-func that takes a function and a value
; and returns the result of applying the function to the value.
(define apply-func
  (λ (f x) (f x)))
