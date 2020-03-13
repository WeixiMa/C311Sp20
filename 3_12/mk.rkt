#lang typed/racket     

(struct var
  ([name : Symbol])
  #:transparent)

(define-type term
  (U var
     Symbol
     Null
     (Pairof term term)))

(define-type Substitution
  (Listof (Pairof var term)))

(: walk (→ term Substitution
           term))
(define walk
  (λ (v s)
    (let ([a (and (var? v) (assv v s))])
      (cond
        [(pair? a) (walk (cdr a) s)]
        [else v]))))

#|an example of substitution|#
#;
(let ([x (var 'x)]
      [y (var 'y)]
      [z (var 'z)])
  (walk x `((,x . ,z) (,y . ,x) (,z . (,x ,x ,x)))))

(: ext-s (→ var term Substitution
            (U Substitution False)))
(define ext-s
  (λ (x v s)
    (cond
      [(occurs? x v s) #f]
      [else (cons `(,x . ,v) s)])))

(: occurs? (→ var term Substitution
              Boolean))
(define occurs?
  (λ (x v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) (eqv? v x)]
        [(pair? v)
         (or (occurs? x (car v) s)
             (occurs? x (cdr v) s))]
        [else #f]))))

#|this ext-s fails (returns #f) because it violates occurrence check|#
#;
(let ([x (var 'x)]
      [y (var 'y)])
  (let ([s `((,y . (,x)))])
    (ext-s x `(a ,y) s)))

(: unify (→ term term Substitution
            (U Substitution False)))
(define unify
  (λ (u v s)
    (let ([u (walk u s)]
          [v (walk v s)])
      (cond
        [(eqv? u v) s]
        [(var? u) (ext-s u v s)]
        [(var? v) (ext-s v u s)]
        [(and (pair? u) (pair? v))
         (let ([s (unify (car u) (car v) s)])
           (and s (unify (cdr u) (cdr v) s)))]
        [else #f]))))

(define-type (Stream A)
  (U Null
     (Pairof A (Stream A))
     (→ (Stream A))))

(define-type Goal
  (→ Substitution (Stream Substitution)))

(: ≡ (→ term term
        Goal))
(define ≡
  (λ (u v)
    (λ (s)
      (let ([s (unify u v s)])
        (if s `(,s) '())))))

(: disj₂ (→ Goal Goal
            Goal))
(define (disj₂ g₁ g₂)
  (λ (s)
    (append-inf (g₁ s) (g₂ s))))

(: append-inf (All (A) (→ (Stream A) (Stream A)
                          (Stream A))))
(define append-inf
  (λ ($₁ $₂)
    (cond
      [(null? $₁) $₁]
      [(pair? $₁)
       (cons (car $₁) (append-inf (cdr $₁) $₂))]
      [else
       (λ () (append-inf $₂ ($₁)))])))

(: conj₂ (→ Goal Goal
            Goal))
(define conj₂
  (λ (g₁ g₂)
    (λ (s)
      (append-map-inf g₂ (g₁ s)))))

(: append-map-inf (All (A) (→ (→ A (Stream A)) (Stream A)
                              (Stream A))))
(define append-map-inf
  (λ (g $)
    (cond
      [(null? $) '()]
      [(pair? $) (append-inf (g (car $))
                             (append-map-inf g (cdr $)))]
      [else
       (λ () (append-map-inf g ($)))])))


(: take-inf (All (A) (→ (U Number False) (Stream A)
                        (Listof A))))
(define take-inf
  (λ (n $)
    (cond
      [(and n (zero? n)) '()]
      [(null? $) '()]
      [(pair? $)
       (cons (car $)
             (take-inf (and n (sub1 n))
                       (cdr $)))]
      [else (take-inf n ($))])))

(: succeed Goal)
(define succeed
  (λ (s)
    `(,s)))

(: fail Goal)
(define fail
  (λ (s)
    '()))

(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj g) g]
    [(disj g₀ g ...) (disj₂ g₀ (disj g ...))]))

(define-syntax conj
  (syntax-rules ()
    [(conj) succeed]
    [(conj g) g]
    [(conj g₀ g ...) (conj₂ g₀ (conj g ...))]))

(define-syntax conde
  (syntax-rules ()
    [(conde (g ...) ...)
     (disj (conj g ...) ...)]))

((let ([x (var 'x)]
        [y (var 'y)])
   (≡ `(,x ,y) `(cat cat)))
 '())
