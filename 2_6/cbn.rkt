#lang typed/racket

#|This is a call-by-name interpreter.|#

(define-type Exp
  (U Symbol
     Number
     (List '+ Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))
(define-type Env
  (Listof (Pairof Symbol (→ Val))))
(define-type Closure
  (→ (→ Val) Val))
(define-type Val
  (U Number
     Boolean
     Closure
     Void))
(: init-env Env)
(define init-env '())
(: ext-env (→ Symbol (→ Val) Env Env))
(define ext-env
  (λ (x th env)
    `((,x . ,th) . ,env)))
(: apply-env (→ Env Symbol Val))
(define apply-env
  (λ (env y)
    (match env
      ['() (error "unbound " y)]
      [`((,x . ,th) . ,env)
       (if (eqv? y x) (th) (apply-env env y))])))

(: valof (→ Exp Env
            Val))
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (apply-env env y)]
      [`,n
       #:when (number? n)
       (begin (displayln 'got-a-number)
              n)]
     [`(+ ,e₁ ,e₂)
       (let ([r₁ (valof e₁ env)]
             [r₂ (valof e₂ env)])
         (cond
           [(and (number? r₁) (number? r₂))
            (+ r₁ r₂)]
           [else (error "don't be silly")]))] 
      [`(λ (,x) ,body)
       (λ ([th : (→ Val)]) (valof body (ext-env x th env)))]
      [`(,rator ,rand)
       (let ([clos (valof rator env)]
             [th (λ () (valof rand env))])
         (cond
           [(or (number? clos) (boolean? clos) (void? clos)) (error "don't be silly")]
           [else (clos th)]))])))

(valof '((λ (x) (+ x x)) 5)
       init-env)

#;
(valof '((λ (x) 5) ((λ (x) (x x)) (λ (x) (x x))))
       init-env)

#;
(valof '((λ (x) ((λ (y) (+ x y)) x)) 10)
       init-env)
