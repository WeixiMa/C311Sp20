#lang typed/racket

(define-type Exp
  (U Symbol
     Number
     Boolean
     (List '+ Exp Exp)
     (List '- Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))
#|An environment can look up a symbol and return a Val|#
(define-type Env
  (→ Symbol Val))

(define-type Closure
  (→ Val Val))
(define-type Val
  (U Number
     Boolean
     Closure))

#|An interpreter converts an Exp together with an Env to a Val|#
(: valof (→ Exp Env
            Val))
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`,n
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
     [`(+ ,e₁ ,e₂)
       (let ([r₁ (valof e₁ env)]
             [r₂ (valof e₂ env)])
         (cond
           [(and (number? r₁) (number? r₂))
            (+ r₁ r₂)]
           [else (error "don't be silly")]))] 
     [`(- ,e₁ ,e₂)
       (let ([r₁ (valof e₁ env)]
             [r₂ (valof e₂ env)])
         (cond
           [(and (number? r₁) (number? r₂))
            (- r₁ r₂)]
           [else (error "don't be silly")]))]
      [`(λ (,x) ,body)
       (λ ([a : Val]) (valof body (λ ([y : Symbol]) (if (eqv? y x) a (env y)))))]
      [`(,rator ,rand)
       (let ([clos (valof rator env)]
             [a (valof rand env)])
         (cond
           [(or (number? clos) (boolean? clos)) (error "don't be silly")]
           [else (clos a)]))])))

#;
(valof '(((λ (x) (λ (y) (+ x y))) 3) 5)
       (λ (y) (error "oops")))
(valof '(((λ (x) (λ (y) (- y x))) 5) 6)
       (λ (y) (error "oops")))
#;
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`,n
       #:when (number? n)
       n]
      [`(λ (,x) ,body)
       (λ (a) (valof body (λ (y) (if (eqv? y x) a (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))
