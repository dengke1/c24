(module evaluator scheme

  ; some helper functions that should make your code much easier to read
  (define unary-op first)
  (define operand second)
  (define binary-op second)
  (define left first)
  (define right third)
  (define (atomic? v) (not (list? v)))
  (define (unary? expr) (= 2 (length expr)))
  (define (binary? expr) (= 3 (length expr)))

  
  ; It is a good idea to define more such helpers here...
  
  
  (define (cont context dir expr)
    (dict-ref context (dir expr)))
  
  ; (ev-not expr context) -> boolean
  ; takes only (not atomic) with context
  (define (ev-not expr context)
    (cond 
      [(boolean? (second expr)) (not (second expr))]
      [(not (boolean? (second expr))) (not (cont context second expr))]))
  
  ; (ev-or expr context) -> boolean
  ; takes only (atomic or atomic) with context and evaluates
  (define (ev-or expr context)
    (cond
      ; checks which symbols need to be substituted from context
      [(and (boolean? (left expr)) (boolean? (right expr))) (or (left expr) (right expr))]
      [(and (not (boolean? (left expr))) (boolean? (right expr))) (or (cont context left expr) (right expr))]
      [(and (boolean? (left expr)) (not (boolean? (right expr)))) (or (left expr) (cont context right expr))]
      [(and (not (boolean? (left expr))) (not(boolean? (right expr)))) (or (cont context left expr) (cont context right expr))]))
  
  ; (ev-and expr context) -> boolean
  ; takes only (atomic and atomic) with context
  (define (ev-and expr context)
    (cond 
      ; checks which symbols need to be substituted from context
      [(and (boolean? (left expr)) (boolean? (right expr))) (and (left expr) (right expr))]
      [(and (not (boolean? (left expr))) (boolean? (right expr))) (and (cont context left expr) (right expr))]
      [(and (boolean? (left expr)) (not (boolean? (right expr)))) (and (left expr) (cont context right expr))]
      [(and (not (boolean? (left expr))) (not(boolean? (right expr)))) (and (cont context left expr) (cont context right expr))]))
  
  ; (ev expr context op) -> boolean
  ; determines which operand to evaluate expression with
  (define (ev expr context op)
    (cond 
      [(eqv? op 'and) (ev-and expr context)]
      [(eqv? op 'or) (ev-or expr context)]))

  ; (evaluate expr context) -> boolean
  ; expr: a valid representation of an expression
  ; context: list of pairs: symbol to #t/#f
  ; Return the value of expr with values of all variables
  ; that occur in expr specified in context.
  (define evaluate
    (lambda (expr context)
     (cond
        ;base cases
        [(and (unary? expr) (atomic? (second expr))) (ev-not expr context)]
        [(and (binary? expr) (atomic? (left expr)) (atomic? (right expr))) (ev expr context (binary-op expr))]
        ; recursive steps
        [(and (unary? expr) (list? (second expr))) 
         (ev-not (list (unary-op expr) (evaluate (second expr) context)) context)]
        [(and (binary? expr) (list? (left expr))) 
         (ev (list (evaluate (left expr) context) (binary-op expr) (right expr)) context (binary-op expr))]
        [(and (binary? expr) (list? (right expr))) 
         (ev (list (left expr) (binary-op expr) (evaluate (right expr) context)) context (binary-op expr))])))

  ; (simplify-or expr) -> primitive
  ; simplifies (atomic or atomic)
  (define (simplify-or expr)
    (cond
      [(or (eqv? #t (left expr)) (eqv? #t (right expr)))] 
      [(or (eqv? #f (left expr))) (right expr)]
      [(or (eqv? #f (right expr))) (left expr)]
      [else expr]))
  
  ; (simplify-and expr) -> primitive
  ; simplifies (atomic and atomic)
  (define (simplify-and expr)
    (cond
      [(or (eqv? #f (left expr)) (eqv? #f (right expr))) (and #f #f)] ; i just want to return a bool
      [(or (eqv? #t (left expr))) (right expr)]
      [(or (eqv? #t (right expr))) (left expr)]
      [else expr]))

  
  ; (replace expr context) -> expr
  ; replaces symbols in expression with bool if possible
  ; takes only (atomic and/or atomic)
  (define (replace expr context)
    (map (λ (symbol) (if (dict-has-key? context symbol)
                                 (dict-ref context symbol)
                                 symbol)) 
         expr))
 

  ; (simplify-not expr context) -> expr
  ; if able, evaluate, otherwise return old expr
  (define (simplify-not expr context)
   (if (dict-has-key? context (second expr))
       (ev-not expr context)
       expr))
  
 
  ; (sim expr op) -> expr
  ; decides which simplify func to use
  (define (sim expr op)
    (cond 
      [(eqv? op 'and) (simplify-and expr)]
      [(eqv? op 'or) (simplify-or expr)]))
  
  
  ; (simplify expr context) -> valid expression
  ; expr: a valid representation of an expression
  ; context: list of pairs: symbol to #t/#f
  ; Return an expression that is equivalent to expr,
  ; but is simplified as much as possible, according to
  ; the given rules.
 (define simplify
    (lambda (expr context)
     (cond
        ;base cases
        [(and (unary? expr) (atomic? (second expr)) ) (simplify-not expr context)]
        [(and (binary? expr) (atomic? (left expr)) (atomic? (right expr))) (sim (replace expr context) (binary-op expr))]
        ; recursive steps
        [(and (unary? expr) (list? (second expr))) (simplify-not (list (unary-op expr) (simplify (second expr) context)))]
        [(and (binary? expr) (list? (left expr)) (list? (right expr))) 
         (simplify (list (simplify (left expr) context) (binary-op expr) (simplify (right expr) context)) context)]
        [(and (binary? expr) (list? (left expr))) 
         (sim (replace (list (simplify (left expr) context) (binary-op expr) (right expr)) context) (binary-op expr))]
        [(and (binary? expr) (list? (right expr))) 
         (sim (replace (list (left expr) (binary-op expr) (simplify (right expr) context)) context) (binary-op expr))])))
  
  (provide evaluate simplify) 
  )
