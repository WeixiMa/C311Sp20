#lang typed/racket

(define-type Exp
  (U Symbol
     Number
     (List '+ Exp Exp)
     (List 'begin Exp Exp)
     (List 'set! Symbol Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))
(define-type Env
  (Listof (Pairof Symbol (Boxof Val))))
(define-type Closure
  (→ (Boxof Val) Val))
(define-type Val
  (U Number
     Boolean
     Closure
     Void))
(: init-env Env)
(define init-env '())
(: ext-env (→ Symbol (Boxof Val) Env Env))
(define ext-env
  (λ (x b env)
    `((,x . ,b) . ,env)))
(: apply-env (→ Env Symbol Val))
(define apply-env
  (λ (env y)
    (match env
      ['() (error "unbound " y)]
      [`((,x . ,(box v)) . ,env)
       (if (eqv? y x) v (apply-env env y))])))

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
       n]
      [`(begin ,e₁ ,e₂)
       (begin (valof e₁ env)
              (valof e₂ env))]
      [`(set! ,x ,e)
       (let ([v (valof e env)])
         (cond
           [(assv x env)
            =>
            (λ ([pr : (Pairof Symbol (Boxof Val))])
              (set-box! (cdr pr) v))
            ]
           [else (error "oops")]))]
     [`(+ ,e₁ ,e₂)
       (let ([r₁ (valof e₁ env)]
             [r₂ (valof e₂ env)])
         (cond
           [(and (number? r₁) (number? r₂))
            (+ r₁ r₂)]
           [else (error "don't be silly")]))] 
      [`(λ (,x) ,body)
       (λ ([a : (Boxof Val)]) (valof body (ext-env x a env)))]
      [`(,rator ,x)
       #:when (symbol? x)
       (cond
         [(assv x env)
          =>
          (λ ([pr : (Pairof Symbol (Boxof Val))])
            (let ([clos (valof rator env)]
                  [b (cdr pr)])
              (cond
                [(or (number? clos) (boolean? clos) (void? clos)) (error "don't be silly")]
                [else (clos b)])))]
         [else (error "unbound " x)])]
      [`(,rator ,rand)
       (let ([clos (valof rator env)]
             [a (valof rand env)])
         (cond
           [(or (number? clos) (boolean? clos) (void? clos)) (error "don't be silly")]
           [else (clos (box a))]))])))

(valof '((λ (x) ((λ (y) (begin (set! y 5) y)) x)) 10)
       init-env)
