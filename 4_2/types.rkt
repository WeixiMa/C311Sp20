#lang racket
(require "mkr.rkt")

(define Γ₀
  '((x . ℕ) (x . 𝔹) (z . ℕ)))

(defrel (lookupᵒ Γ x t)
  (fresh (x^ t^ Γ^)
    (≡ `((,x^ . ,t^) . ,Γ^) Γ)
    (conde
     [(≡ x^ x) (≡ t^ t)]
     [(≠ x^ x)
      (lookupᵒ Γ^ x t)])))

#;
(run 2 q
  (lookupᵒ Γ₀ 'x q))

#;
(run 1 τ
  (lookupᵒ Γ₀ 'x τ))

(defrel (⊢ Γ e τ)
  (conde
   [(symbolo e)
    (lookupᵒ Γ e τ)]
   [(fresh (x body)
      (== `(fix (λ (,x) ,body)) e)
      (⊢ `((,x . ,τ) . ,Γ) body τ))]
   [(numbero e)
    (== τ 'ℕ)]
   [(conde
     [(== e #t)]
     [(== e #f)])
    (== τ '𝔹)]
   [(fresh (e₁ e₂)
      (== `(* ,e₁ ,e₂) e)
      (⊢ Γ e₁ 'ℕ)
      (⊢ Γ e₂ 'ℕ)
      (== τ 'ℕ))]
   [(fresh (e₁ e₂ e₃)
      (== `(if ,e₁ ,e₂ ,e₃) e)
      (⊢ Γ e₁ '𝔹)
      (⊢ Γ e₂ τ)
      (⊢ Γ e₃ τ))]
   [(fresh (e₁)
      (== `(sub1 ,e₁) e)
      (⊢ Γ e₁ 'ℕ)
      (== τ 'ℕ))]
   [(fresh (e₁)
      (== `(zero? ,e₁) e)
      (⊢ Γ e₁ 'ℕ)
      (== τ '𝔹))]
   [(fresh (x body)
      (== `(λ (,x) ,body) e)
      (symbolo x)
      (fresh (τin τout)
        (== `(→ ,τin ,τout) τ)
        (⊢ `((,x . ,τin) . ,Γ) body τout)))]
   [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (τin)
        (⊢ Γ rator `(→ ,τin ,τ))
        (⊢ Γ rand τin)))]
   ))

#;
(run 10 τ
  (⊢ '()
     '(λ (y) (zero? ((λ (x) (sub1 y)) 4)))
     τ))

#;
(run 1 τ
  (⊢ '() `((fix (λ (fact)
                  (λ (n)
                    (if (zero? n)
                        1
                        (* n (fact (sub1 n)))))))
           5)
     τ))

(defrel (lookup2ᵒ vars vals x o)
  (fresh (var val vars^ vals^)
    (≡ `(,var . ,vars^) vars)
    (≡ `(,val . ,vals^) vals)
    (conde
     [(≡ var x) (≡ val o)]
     [(≠ var x)
      (lookup2ᵒ vars^ vals^ x o)])))

(defrel (valof*ᵒ vars vals es o)
  (conde
   [(== '() es)
    (== '() o)]
   [(fresh (a d)
      (== `(,a . ,d) es)
      (fresh (aᵒ dᵒ)
        (== `(,aᵒ . ,dᵒ) o)
        (valofᵒ vars vals a aᵒ)
        (valof*ᵒ vars vals d dᵒ)))]))

(defrel (valofᵒ vars vals e val)
  (conde
   [(symbolo e)
    (lookup2ᵒ vars vals e val)]
   [(== `(quote ,val) e)
    (absentᵒ 'clos val)]
   [(fresh (es)
      (== `(list . ,es) e)
      (absentᵒ 'list vars)
      (valof*ᵒ vars vals es val))]
   [(fresh (x body)
      (== `(λ (,x) ,body) e)
      (symbolo x)
      (=/= 'quote x)
      (=/= 'λ x)
      (== `(clos ,x ,body ,vars ,vals) val))]
   [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (x body vars^ vals^ a)
        (valofᵒ vars vals rator `(clos ,x ,body ,vars^ ,vals^))
        (valofᵒ vars vals rand a)
        (valofᵒ `(,x . ,vars^) `(,a . ,vals^) body val)))]))

(defrel (quine e)
  (valofᵒ '() '() e e))

(defrel (twine e₁ e₂)
  (=/= e₁ e₂)
  (valofᵒ '() '() e₁ e₂)
  (valofᵒ '() '() e₂ e₁))

(defrel (thrine e₁ e₂ e₃)
  (=/= e₁ e₂)
  (=/= e₂ e₃)
  (valofᵒ '() '() e₁ e₂)
  (valofᵒ '() '() e₂ e₃)
  (valofᵒ '() '() e₃ e₁))

(run 10 e
  (quine e))

