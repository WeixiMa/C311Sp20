#lang typed/racket

#|
bi-directional type checking
check: Γ ⊢ e ∈ τ, Γ e τ all inputs, boolean output
- constructors are annoated and checked

synth: Γ ⊢ e ↑ τ, Γ e inputs, τ output
- eliminators are synthed


Γ ⊢ e : τ

α
β
|#

#|
Exercises:
1, add the Nat type (Nat, zero, add1, rec-Nat)
2, add the List type ((List A), nil, ::, rec-List)
3, add the Σ type  (basically copying the Π type)
|#

(: lookup (All (A D) (→ (Listof (Pairof A D)) A D)))
(define (lookup ρ x)
  (match ρ
    ['() (error "unbound " x)]
    [`((,a . ,d) . ,ρ^)
     (if (eqv? a x) d (lookup ρ^ x))]))
(: ext (All (A D) (→ (Listof (Pairof A D)) A D (Listof (Pairof A D)))))
(define (ext l a d)
  `((,a . ,d) . ,l))

(define-type Exp
  (U (List 'Π (List (List Symbol Exp)) Exp) #|(Π ([x A]) B)|#
     (List 'λ (List Symbol) Exp)            #|(λ (x) b)|#
     (List Exp Exp)                         #|(rator rand)|#
     'U
     Symbol
     ))

(: β (→ Exp Symbol Exp
        Exp))
(define (β e x t)
  (match e
    [`,y
     #:when (symbol? y)
     (if (eqv? y x) t y)]
    ['U 'U]
    [`(λ (,y) ,b)
     (cond
       #|
       There is a third case: y occurs free in t.
       Then you need to α-rename y before the recursion on b.
       |#
       [(eqv? y x)
        #|(x (λ (x) x)) x y|#
        `(λ (,y) ,b)]
       [else
        #|(x (λ (z) x)) x y|#
        `(λ (,y) ,(β b x t))])]
    [`(Π ([,y ,A]) ,B)
     (cond
       #|
       There is a third case: y occurs free in t.
       Then you need to α-rename y before the recursion on B.
       |#
       [(eqv? y x)
        `(Π ([,y ,(β A x t)]) ,B)]
       [else
        `(Π ([,y ,(β A x t)]) ,(β B x t))])]
    [`(,rator ,rand)
     `(,(β rator x t) ,(β rand x t))]))


(define-type Context #|we use Γ for contexts|#
  (Listof (Pairof Symbol Exp)))

(: synth (→ Context Exp
            (U Exp False)))
(define (synth Γ e)
  (match e
    [`,y
     #:when (symbol? y)
     (lookup Γ y)]
    ['U 'U]
    [`(Π ([,x ,A]) ,B)
     (go-on ([_ (check Γ A 'U)]
             [_ (check (ext Γ x A) B 'U)])
       'U)]
    [`(,rator ,rand)
     (go-on ([`(Π ([,x ,A]) ,B) (synth Γ rator)]
             [_ (check Γ rand A)])
       #|
       (f 5) 
       f : (Π ([n Nat]) (Vec Atom n))
       5 : Nat
       (Vec Atom 5)
       |#
       (β B x rand))]))

(: check (→ Context Exp Exp
            Boolean))
(define (check Γ e τ)
  (match `(,e ,τ)
    [`((λ (,x) ,b) (Π ([,x^ ,A]) ,B))
     #|e.g. (λ (x) x) (Π ([a Nat]) Nat)|#
     #|e.g. (λ (x) x) (Π ([n Nat]) (Vec Atom n))|#
     (check (ext Γ x A) b (β B x^ x))]
    [`(,elim ,τ)
     (go-on ([τ^ (synth Γ elim)])
       (aeq? τ τ^))]))

(define-type DExp
  #|de~Bruijn Exp|#
  (U (List 'Π (List DExp) DExp)
     (List 'λ DExp)
     (List DExp DExp)
     'U
     Symbol
     Number))

(: lex (→ Exp (Listof Symbol)
          DExp))
(define (lex e names)
  (match e
    [`,y
     #:when (symbol? y)
     (or (index-of names y) y)]
    [`(Π ([,x ,A]) ,B)
     `(Π (,(lex A names)) ,(lex B (cons x names)))]
    [`(λ (,x) ,b)
     `(λ ,(lex b (cons x names)))]
    [`(,rator ,rand)
     `(,(lex rator names) ,(lex rand names))]
    ['U 'U]))

(: aeq? (→ Exp Exp Boolean))
(define (aeq? e₁ e₂)
  (equal? (lex e₁ '())
          (lex e₂ '())))

(define-type UserExp
  (U 'U
     (List 'λ (Listof Symbol) UserExp)
     (List 'Π (Listof (List Symbol UserExp)) UserExp)
     (Listof UserExp)
     Symbol))

(: pre (→ UserExp Exp))
(define (pre e)
  (match e
      [`(Π () ,e)
       (pre e)]
      [`(Π ([,x ,e₁] . ,d) ,e₂)
       #:when (symbol? x)
       `(Π ([,x ,(pre e₁)])
          ,(pre `(Π ,d ,e₂)))]
      [`(λ () ,e)
       (pre e)]
      [`(λ (,x . ,d) ,e)
       #:when (symbol? x)
       `(λ (,x)
          ,(pre `(λ ,d ,e)))]
      ['U 'U]
      [`,x
       #:when (symbol? x)
       x]
      [`(,f . ,d)
       (let ([d (map (λ ([e : UserExp]) (pre e)) d)])
         (foldl (λ ([e : Exp]
                    [r : Exp])
                  `(,r ,e))
                (pre f)
                d))]))

(: run (→ UserExp UserExp Boolean))
(define (run e t)
  (check '() (pre e) (pre t)))

#;
(let ([t '(Π ([A U]
              [a A])
            A)]
      [e '(λ (A a)
            a)])
  (run e t))

(let ([t '(Π ([ℕ U]
              [Vec (Π ([X U]
                       [n ℕ])
                     U)]
              [+ (Π ([n₁ ℕ]
                     [n₂ ℕ])
                   ℕ)]
              [append (Π ([X U]
                          [n₁ ℕ]
                          [n₂ ℕ]
                          [v₁ (Vec X n₁)]
                          [v₂ (Vec X n₂)])
                        (Vec X (+ n₁ n₂)))]
              [X U]
              [n₁ ℕ]
              [n₂ ℕ]
              [v₁ (Vec X n₁)]
              [v₂ (Vec X n₂)])
            (Vec X (+ n₁ n₂)))]
      [e '(λ (ℕ Vec + append X n₁ n₂ v₁ v₂)
            (append X n₁ n₂ v₁ v₂))])
  (run e t))

































(define-syntax go-on
  (syntax-rules ()
    [(go-on () r) r]
    [(go-on ([p₀ e₀] [p e] ...) r)
     (cond
       [e₀
        =>
        (λ (v)
          (match v
            [p₀ (go-on ([p e] ...) r)]))]
       [else #f])]))

