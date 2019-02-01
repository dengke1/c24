(module lab6 scheme
  
;;; (my-reverse xs) -> list
;;; xs: list
;;; Return the reverse of xs. (Not tail-recursive.)
(define my-reverse-1
  (λ (xs)
    (if (empty? xs)
        '()
        (append (my-reverse-1 (rest xs)) (list (first xs)) ))))

;;; tail-recursive version
(define my-reverse-2
  (λ (xs)
    (letrec ([tail-rec
              (λ (xs res)
                (if (empty? xs)
                res
                (tail-rec (rest xs) (cons (first xs) res))))])
      (tail-rec xs '()))))

;;; (num-elts xs) -> int
;;; xs: list
;;; Return the number of elements in xs, including
;;; any sublists, on any nesting level.
;;; using recursion only, no tail recursion
(define num-els-1
  (λ (xs)
    (if (empty? xs)
        0
        (if (list? (first xs))
               (+ (num-els-1 (first xs)) (num-els-1 (rest xs)))
               (+ 1 (num-els-1 (rest xs)))))))

;;; using HOPs (and recursion)
;;; this solution should be much shorter!
(define num-els-2
  (λ (xs)
    (if (not (list? xs))
         1
         (apply + (map num-els-2 xs)))))

;;; tail-recursive version
(define num-els-3
  (λ (xs)
    (letrec ([nm-tail (λ (xs res) (if (empty? xs)
                                      res
                                      (if (list? (first xs))
                                          (nm-tail (first xs) (+ res (nm-tail (rest xs) 0)))
                                          (nm-tail (rest xs) (+ 1 res)))))])
      (nm-tail xs 0))))


;;; (flatten xs) -> list
;;; xs: list
;;; Return the flattened version of xs. 
;;; using recursion only, no tail recursion
(define flatten-1
  (λ (xs)
    (if (empty? xs)
        '()
        (if (list? (first xs))
            (append (flatten-1 (first xs)) (flatten-1 (rest xs)))
            (append (list (first xs)) (flatten-1 (rest xs)))))))

;;; using HOPs (and recursion)
(define flatten-2
  (λ (xs)
    (if (list? xs)
        (apply append (map flatten-2 xs))
        (list xs))))

;;; tail-recursive version
(define flatten-3
  (λ (xs)
    (letrec ([tail-rec (λ (xs res) 
                         (if (empty? xs)
                             res
                             (if (list? (first xs))
                                 (append (tail-rec (first xs) (tail-rec (rest xs) '())) res)
                                 (append (list (first xs)) (tail-rec (rest xs) res)))))])
      (tail-rec xs '()))))

(provide my-reverse-1 my-reverse-2 num-els-1 num-els-2 num-els-3 flatten-1 flatten-2 flatten-3)
)
