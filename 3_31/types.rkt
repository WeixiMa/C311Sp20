#lang racket
(require "mk.rkt")

(define Γ₀
  '((x . ℕ) (y . Bool) (z . ℕ)))

(defrel (lookupᵒ Γ x t)
  (fresh (x^ t^ Γ^)
    (== `((,x^ . ,t^) . ,Γ^) Γ)
    (conde
     [(== x^ x) (== t^ t)]
     [(=/= x^ x)
      (lookupᵒ Γ^ x t)])))

#;
(run 1 τ
  (lookupᵒ Γ₀ 'x τ))

(defrel (⊢ Γ e τ)
  (conde
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
   [(symbolo e)
    (lookupᵒ Γ e τ)]
   [(fresh (x body)
      (== `(λ (,x) ,body) e)
      (fresh (τin τout)
        (== `(→ ,τin ,τout) τ)
        (⊢ `((,x . ,τin) . ,Γ) body τout)))]
   [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (τin)
        (⊢ Γ rator `(→ ,τin ,τ))
        (⊢ Γ rand τin)))]
   [(fresh (x body)
      (== `(fix (λ (,x) ,body)) e)
      (⊢ `((,x . ,τ) . ,Γ) body τ))]))



#|self application does NOT have a type|#
#;
(run 1 τ
  (⊢ '() '(fix (λ (fact)
                 (λ (n)
                   (if (zero? n)
                       1
                       (* n (fact (sub1 n)))))))
     τ))

#;
(((λ (x) (x x))
  (λ (fact)
    (λ (n)
      (if (zero? n)
          1
          (* n ((fact fact) (sub1 n)))))))
 5)


#|Type checking does not require running the program (at least here).|#
(run 1 τ
  (⊢ '() '((fix (λ (fact)
                  (λ (n)
                    (* n (fact (sub1 n))))))
           5)
     τ))

#|
Try to add Pair type and List type to this small language.
And make up the inference rules for those operators (kons, kar, kdr, etc).
|#

#|
Further reading: Hindley-Milner type system.
|#
