#lang pie

#|

warriors ⇒  software engineers : programs

mages ⇒  mathematicians : proofs

witchers ⇒ PL researchers
|#

#|
Dependent type theory is also called constructive type theory.
|#

#|
For each type, there are four questions:
1, (Formation) how to make such a type
2, (Construction) how to make a member (inhabitant) of this type
3, (Elimination) how to use a member of this type
4, (Equality) how to compare members of this type
|#

#|
Judgments
A is a U
A is the same U as B
|#

#|
the type Nat
1, Nat is a type
2, - zero is a Nat
   - if n is a Nat, then (add1 n) is a Nat
3, - (which-Nat n base step)
     where step is a function, (λ (n-1) ...), this function will be applied
    to n-1, if n takes the form (add1 n-1)
   - (rec-Nat n base step)
     where step is a function, (λ (n-1 almost) ...),
    almost represents the recursion result, this function will be applied
    to n-1 and the recursive result, if n takes the form (add1 n-1)

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
unfinshed
|#

#|
the type (List A)
unfinished
|#

(claim five Nat)
(define five 5)

(claim zero?
  (→ Nat
    Atom))
(define zero?
  (λ (n)
    (which-Nat n
      'yes
      (λ (n-1)
        'no))))

#|
(+ 3 5)
almost: (+ 2 5)

|#

(claim +
  (→ Nat Nat
     Nat))
(define +
  (λ (n m)
    (rec-Nat n
      m
      (λ (n-1 almost)
        #| almost is the natural recursion, (+ n-1 m) |#
        (add1 almost)
        ))
    #|In dependent types, EVERY program has to TERMINATE.|#
    #;
    (cond
      [(zero? n) m]
      [else (add1 (+ (sub1 n) m))])))

#|
(gauss 10) -> 55
almost: (gauss 9) -> 45

|#

(claim gauss
  (→ Nat
    Nat))
(define gauss
  (λ (n)
    (rec-Nat n
      0
      (λ (n-1 almost)
        (+ almost (add1 n-1))))))

(claim id₁
  (→ Nat
    Nat))
(define id₁
  (λ (n)
    n))

(claim id₂
  (→ Nat
    Nat))
(define id₂
  (λ (x)
    x))

(claim id₃
  (→ Nat
    Nat))
(define id₃
  (λ (m)
    (id₁ m)))

(claim id₄
  (→ U
    U))
(define id₄
  (λ (A)
    A))

#;
(claim repeat
  (→ A Nat
     (List Atom)))
#;
(define repeat
  (λ (a n)
    (rec-Nat n
      (the (List Atom) nil)
      (λ (n-1 almost)
        (:: a almost)))))

#;
(claim repeat-nat
  (→ Nat Nat
     (List Nat)))
#;
(define repeat-nat
  (λ (a n)
    (rec-Nat n
      (the (List Nat) nil)
      (λ (n-1 almost)
        (:: a almost)))))

(claim repeat
  (Π ([A U])
    (→ A Nat
       (List A))))
(define repeat
  (λ (A)
    (λ (a n)
      (rec-Nat n
        (the (List A) nil)
        (λ (n-1 almost)
          (:: a almost))))))
