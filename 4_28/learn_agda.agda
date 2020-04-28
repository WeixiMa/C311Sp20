module learn_agda where

open import Data.Product
-- learn more about Identity type
-- learn Agda!

data ℕ : Set where
  zero : ℕ
  add1 : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + m = m
add1 n + m = add1 (n + m)

-- A is a parameter
-- ℓ is an index
data Vect (A : Set) : (ℓ : ℕ) → Set where
  nil : Vect A zero
  cons : ∀ {n : ℕ} → A → Vect A n → Vect A (add1 n)

one : ℕ
one = add1 zero

some-vec : Vect ℕ (add1 zero)
some-vec = cons one nil

vec-append : ∀ {A : Set} → {n m : ℕ}
             → Vect A n → Vect A m
             → Vect A (n + m)
vec-append nil v₂ = v₂
vec-append (cons a v₁) v₂ = cons a (vec-append v₁ v₂)

data _≡_ {A : Set} (x : A) : (y : A) → Set where
  same : x ≡ x

symm : ∀ {A} {x y : A}
       → x ≡ y
       → y ≡ x
symm same = same

cong : ∀ {A B} {x y : A}
       → x ≡ y → (f : A → B)
       → (f x) ≡ (f y)
-- goal before pattern matching: f x ≡ f y
-- goal after pattern matching: f x ≡ f x
cong same f = same

add1-jumps-in : ∀ (n m : ℕ) → (add1 (n + m)) ≡ (n + (add1 m))
add1-jumps-in zero m = same
add1-jumps-in (add1 n) m with add1-jumps-in n m
... | ih = cong ih add1

add1-jumps-out : ∀ (n m : ℕ) → (n + (add1 m)) ≡ (add1 (n + m))
add1-jumps-out n m = symm (add1-jumps-in n m)

one-is-one : (add1 zero) ≡ (add1 zero)
one-is-one = same

-- replace produces a new type
replace : ∀ {A} {x y : A}
          → (B : A → Set)
          → x ≡ y
          → B x
          → B y
replace B same Bx = Bx

lemma : (n m : ℕ) → n ≡ m → Vect ℕ n → Vect ℕ m
lemma n m n=m v = replace (λ ℓ → Vect ℕ ℓ) n=m v

n=n+0 : ∀ (n : ℕ) → n ≡ (n + zero)
n=n+0 zero = same
n=n+0 (add1 n) with n=n+0 n
... | ih = cong ih add1

+-is-comm : ∀ (n m : ℕ) → (n + m) ≡ (m + n)
+-is-comm zero m = n=n+0 m
+-is-comm (add1 n) m with +-is-comm n m
... | ih = replace (λ x → add1 (n + m) ≡ x) (add1-jumps-in m n) (cong ih add1)

data Either (A B : Set) : Set where
  left  : (a : A) → Either A B
  right : (b : B) → Either A B

Even : ℕ → Set
Even n = Σ[ n/2 ∈ ℕ ](n ≡ (n/2 + n/2))

Odd : ℕ → Set
Odd n = Σ[ n/2 ∈ ℕ ](n ≡ add1 (n/2 + n/2))

3-is-Odd : Odd (add1 (add1 (add1 zero)))
3-is-Odd = (add1 zero) , same

even-or-odd-decidable : ∀ (n : ℕ) → Either (Even n) (Odd n)
even-or-odd-decidable zero = left (zero , same)
even-or-odd-decidable (add1 n) with even-or-odd-decidable n
... | left (n/2 , eq)  = right (n/2 , cong eq add1)
... | right (n/2 , eq) = left (add1 n/2 , (replace (λ x → add1 n ≡ add1 x) (add1-jumps-in n/2 n/2) (cong eq add1)))

-- eq : n ≡ add1 (h + h)
-- (cong eq add1) : add1 n ≡ add1 (add1 (h + h))
-- (replace (λ x → add1 n ≡ add1 x) add1-jumps-in (cong eq add1))
-- goal : add1 n ≡ add1 (h + add1 h)

-- have: Odd 5 -> 2 , 5 ≡ add1 (2 + 2)
-- goal: Even 6 -> (add1 2) , 6 ≡ ((add1 2) + (add1 2))

-- ih : n + m ≡ m + n
-- (cong ih add1) : add1 (n + m) ≡ add1 (m + n)
-- replace (λ x → add1 (n + m) ≡ x) (add1-jumps-in n m) (cong ih add1)
-- goal : add1 (n + m) = (m + add1 n)
