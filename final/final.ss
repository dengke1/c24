(module final scheme
  
  (define cond-apply
    (λ (p f xs) (if (empty? xs)
                    '()
                    (cons (if (p (first xs))
                              (f (first xs))
                              (first xs))
                          (cond-apply p f (rest xs))))))
  
  (define cond-map
    (λ (p f xs) (map (λ (x) (if (p x)
                                (f x)
                                x))
                     xs)))
  
  (define cond-tail
    (λ (p f xs)
      (local [(define c-tail
                (λ (xs res) (if (empty? xs)
                                res
                                (if (p (first xs))
                                    (c-tail (rest xs) (cons (f (first xs)) res))
                                    (c-tail (rest xs) (cons (first xs) res))))))]
        (c-tail (reverse xs) '()))))
  
  (define cond-cps
    (λ (p f xs)
          (local [(define c-cps
                    (λ (xs k) (if (empty? xs)
                                  (k '())
                                  (if (p (first xs))
                                      (c-cps (rest xs) (λ (x) (k (cons (f (first xs)) x))))
                                      (c-cps (rest xs) (λ (x) (k (cons (first xs) x))))))))]
          (c-cps xs identity))))
)