#lang typed/racket

#|
Representation Independence
How to get rid of higher-order functions:

A higher-order function is a function that is passed
as an input to another function, or a function that is
used as the result of another function.


|#

(define-type Exp
  (U Symbol
     Number
     (List '+ Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))
(define-type Env
  (Listof (Pairof Symbol Val))
  #;
  (→ Symbol Val))
(define-type Closure
  (→ Val Val))
(define-type Val
  (U Number
     Boolean
     Closure))

(: init-env (→ Env))
(define init-env
  (λ ()
    '()
    #;
    (λ (y) (error "unbound variable" y))))
(: extend-env (→ Symbol Val Env
                 Env))
(define extend-env
  (λ (x a env)
    `((,x . ,a) . ,env)
    #;
    (λ (y) (if (eqv? y x) a (apply-env env y)))))
(: apply-env (→ Env Symbol
                Val))
(define apply-env
  (λ (env y)
    (match env
      [`() (error "unbound variable" y)]
      [`((,x . ,a) . ,env^) (if (eqv? y x) a (apply-env env^ y))])
    #;
    (env y)))

(: valof (→ Exp Env
            Val))
(define valof
  (λ (exp env)
    (displayln exp)
    (displayln env)
    (match exp
      [`,y
       #:when (symbol? y)
       (apply-env env y)]
      [`,n
       #:when (number? n)
       n]
      [`(+ ,e₁ ,e₂)
       (let ([r₁ (valof e₁ env)]
             [r₂ (valof e₂ env)])
         (cond
           [(and (number? r₁) (number? r₂))
            (+ r₁ r₂)]
           [else (error "adding non-numbers")]))] 
      [`(λ (,x) ,body)
       (λ ([a : Val]) (valof body (extend-env x a env)))]
      [`(,rator ,rand)
       (let ([clos (valof rator env)]
             [a (valof rand env)])
         (cond
           [(or (number? clos) (boolean? clos)) (error "your rator is not a function")]
           [else (clos a)]))])))

(valof '(((λ (x) (λ (y) (+ y x))) 5) 6) (init-env))
