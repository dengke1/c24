(module lab7 scheme
  
  ;;; define a tail-recursive, linear-time foldr without using
  ;;; continuation passing style
  (define (foldr-tail f s xs)
      (letrec([tail (λ (f s xs res)
                      (if (empty? xs)
                          res
                          (tail f s (rest xs) (f (first xs) res))))])
        (tail f s xs s)))
  
  ;;; define a tail-recursive, linear-time foldr using CPS
  (define (foldr-cps f s xs)
      (letrec([cps (λ (f s xs k)
                     (if (empty? xs)
                         (k s)
                         (cps f s (rest xs) 
                              (λ (x) (k (f (first xs) x))))))])
        (cps f s xs identity)))
  
  ; define a tail-recursive linear-time version of map (a simplified version
  ; that works on one list) using CPS
  (define (map-cps f xs)
    (letrec([cps (λ (f xs k)
                   (if (empty? xs)
                       (k '())
                       (cps f (rest xs) 
                            (λ (x) (k (cons (f (first xs)) x))))))])
      (cps f xs identity)))
  
  (provide foldr-tail foldr-cps map-cps)
  )
