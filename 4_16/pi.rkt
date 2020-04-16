#lang pie

(claim +
  (Π ([n Nat]
      [m Nat])
    Nat)
  #;
  (→ Nat Nat
     Nat))
(define +
  (λ (n m)
    (rec-Nat n
      m
      (λ (n-1 almost)
        (add1 almost)))))

(claim gauss
  (→ Nat
    Nat))
(define gauss
  (λ (n)
    (rec-Nat n
      0
      (λ (k gauss-of-k)
        #|if n is 100,
        k = 99, gauss-of-99
        k = 98, ...|#
        (+ gauss-of-k (add1 k))))))

#|
For each type, there are four questions:
1, (Formation) how to make such a type
2, (Construction) how to make a member (inhabitant) of this type
3, (Elimination) how to use a member of this type
4, (Equality) how to compare members of this type
|#


#|
the type Nat
1, Nat is a type
2, - zero is a Nat
- if n is a Nat, then (add1 n) is a Nat
3, - (rec-Nat n base step)
(rec-Nat 0 base step) ≡ base
(rec-Nat (add1 n) base step) ≡ (step n (rec-Nat n base step))
how to type it?
if we have
- n is a Nat
- base is an A
- step is a (→ Nat A A)
then we have
- (rec-Nat n base step) is an A 

- (ind-Nat n motive base step)
motive is a (→ Nat U)
base is a (motive 0)
step is (Π ([k Nat] [ih (motive k)]) (motive n))
4, - zero is the same Nat as zero
- m is the same Nat as n, (add1 m) is the same Nat as (add1 n)
|#

#|
the type (→ A B)
1, if A is a type and B is a type, (→ A B) is a type
2, - if assuming x is an A while b is a B,
then (λ (x) b) is a (→ A B),
3, - if f is a (→ A B) and arg is an A, then (f arg) is a B 
4, - f is (→ A B) and g is (→ A B),
f and g are the same if f is consistently renamed g, α-conversion
- f is (→ A B), (λ (x) (f x)) is the same (→ A B) as f, η-conversion
|#

#|
the type (Π ([x A]) B)
(→ A B) is a syntax sugar for (Π ([x A]) B), if x is not used in B
1, if A is a type and B is a type by assuming x has type A, (Π ([x A]) B) is a type
2, - if assuming x is an A while b is a B,
then (λ (x) b) is a (→ A B),
3, - if f is a (→ A B) and arg is an A, then (f arg) is a B 
4, - f is (→ A B) and g is (→ A B),
f and g are the same if f is consistently renamed g, α-conversion
- f is (→ A B), (λ (x) (f x)) is the same (→ A B) as f, η-conversion
|#

#;
(claim repeat
  (Π ([X U])
    (Π ([n Nat])
      (List X))))

#|
the type (List A)
1, if A is a type, (List A) is a type
2, - nil is a (List A)
- if a is an A and d is a (List A), (:: a d) is a (List A)
3, - (rec-List ls base step)
(rec-List nil base step) ≡ base
(rec-List (:: a d) base step) ≡ (step a d (rec-List n base step))
how to type it?
if we have
- ls is a (List A)
- base is an B
- step is a (→ A (List A) B B)
then we have
- (rec-List n base step) is an B 
4,
|#

(claim length
  (Π ([X U])
    (→ (List X)
      Nat)))
(define length
  (λ (X xs)
    (rec-List xs
      0
      (λ (a d length-of-d)
        (add1 length-of-d)))))

(claim append
  (Π ([X U])
    (→ (List X) (List X)
       (List X))))
(define append
  (λ (X xs ys)
    (rec-List xs
      ys
      (λ (a d append-d-ys)
        append-d-ys))))

(claim append-vector
  (Π ([X U]
      [n Nat]
      [m Nat])
    (→ (Vec X n) (Vec X m)
       (Vec X (+ n m)))))
(define append-vector
  (λ (X n)
    (ind-Nat n
      #|motive, why|#
      (λ (n) (Π ([m Nat])
               (→ (Vec X n) (Vec X m)
                  (Vec X (+ n m)))))
      (λ (m xs ys)
        #|
        givens:
        n : Nat n is 0
        m : Nat
        xs : (Vec X 0)
        ys : (Vec X m)
        goal:
        (Vec X m)
        |#
        ys)
      (λ (k ih #|induction-hypothesis|#)
        (λ (m xs ys)
          #|
          givens:
          k : Nat
          ih : (Π ([m Nat])
          (→ (Vec X k) (Vec X m)
          (Vec X (+ k m))))
          m : Nat
          (ih m) : (→ (Vec X k) (Vec X m)
          (Vec X (+ k m)))
          xs : (Vec X (add1 k))
          (head xs) : X
          (tail xs) : (Vec X k)
          (ih m (tail xs) ys) : (Vec X (+ k m))
          (vec:: (head xs) (ih m (tail xs) ys)) : (Vec X (+ (add1 k) m))
          ys : (Vec X m)
          goal:
          (Vec X (+ (add1 k) m))
          |#
          (vec:: (head xs) (ih m (tail xs) ys)))))))

#;
(append-vector Atom 2 3
               (vec:: 'cat (vec:: 'cat vecnil))
               (vec:: 'dog (vec:: 'dog (vec:: 'dog
                                              vecnil))))

(claim vec→list
  (Π ([X U]
      [ℓ Nat])
    #|------- |#
    (→ (Vec X ℓ)
      (List X))))
(define vec→list
  (λ (X ℓ)
    (ind-Nat ℓ
      (λ (k) (→ (Vec X k)
               (List X)))
      (λ (vec-of-length-0) nil)
      (λ (k ih)
        (λ (v-of-add1-k)
          (:: (head v-of-add1-k) (ih (tail v-of-add1-k))))
        #|
        givens:
        k : Nat
        ih : (→ (Vec X k)
        (List X))
        v (A.K.A v-of-add1-k) : (Vec X (add1 k))
        (tail v) : (Vec X k)
        (ih (tail v)) : (List X)
        (:: (head v) (ih (tail v))) : (List X)
        goal:
        (List X)
        |#
        ))))

(vec→list Atom 5
          (the (Vec Atom 5)
               (vec:: 'cat
                      (vec:: 'cat
                             (vec:: 'dog
                                    (vec:: 'dog
                                           (vec:: 'dog vecnil)))))))
