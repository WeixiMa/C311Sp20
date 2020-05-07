#lang racket

(define append-map-cps
  (λ (f-cps ls k)
    (cond
      [(null? ls) (k '())]
      [else (f-cps (car ls)
                   (λ (ls₁)
                     (append-map-cps f-cps (cdr ls)
                                     (λ (ls₂)
                                       (k (append ls₁ ls₂))))))])))

(call/cc (λ (k)
           (+ (k 1) (k 2) (k 3))))

(define !-
  (λ (G e t)
    (conde
     [(fresh (e₁ e₂ t₁)
        (== `(begin2 ,e₁ ,e₂) e)
        (!- G e₁ t₁)
        (!- G e₂ t))]
     [(fresh (ma f b a)
        (== `(bind-maybe ,ma ,f) e)
        (== `(Maybe ,b) t)
        (!- G ma `(Maybe ,a))
        (!- G f `(,a -> (Maybe ,b))))])))

#|

Γ,x:τ₁ ⊢ b : τ
   Γ   ⊢ e : τ₁
-----------------------------
   Γ   ⊢ (let ((x e)) b) : τ

|#

b -> 'fish
c -> `(fish)
a -> `(fish)

'(((fish) fish (fish)))

#;
(defrel (append-cdrᵒ ls o)
  (conde
   [(== '() ls)
    (== '() o)]
   [(fresh (a d res)
      (== `(,a . ,d) ls)
      (append-cdrᵒ d res)
      (appendᵒ ls res o))]))

(define gcd
  (λ (a b)
    (cond
      [(eqv? a b) (inj-writer a)]
      [(> a b)
       (go-on ([_ (tell `(swapping ,a ,b))])
         (gcd b a))]
      [else (gcd a (- b a))])))

(define at-least-two
  (λ (n)
    (which-Nat n
      'nil
      (λ (n-1)
        (which-Nat n
          'nil
          (λ (n-1)
            't))))))
