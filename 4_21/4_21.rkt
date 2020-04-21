#lang pie

(claim +
  (→ Nat Nat
     Nat))
(define +
  (λ (n m)
    (rec-Nat n
      m
      (λ (n-1 almost)
        (add1 almost)))))

(claim sub1
  (→ Nat
    Nat))
(define sub1
  (λ (n)
    (which-Nat n
      0
      (λ (n-1)
        n-1))))

#|
Identity type
1, (Formation)
if X is a U, a is an X and b is an X, then (= X a b) is a U
2, (Contruction)
(same a) is an element of (= X a a)
3, (Elimination)
- cong
4, (Equality)
|#

#|
Sigma type
1, (Formation)
if X is a U, x is an A, and
assuming x is X gives you Y is a U,
then (Σ ([x X]) Y) is a U
|#

(claim one-is-one
  (= Nat 1 1))
(define one-is-one
  #|refl|#
  (same 1))

(claim one-is-two
  (= Nat 1 2))
#;
(define one-is-two
  TODO)
#|cannot be proved|#

(claim sub1-add1-same
  (Π ([n Nat])
    (= Nat (sub1 (add1 n)) n)))
(define sub1-add1-same
  (λ (n)
    #|This is what pie will do in the backend|#
    #|(= Nat (sub1 (add1 n)) n)|#
    #|(= Nat (which-Nat (add1 n)
    0
    (λ (n-1)
    n-1))
    n)|#
    #|(= Nat n n)|#
    (same n)))

#|
∀ n,m. (+ (add1 n) m) ≡ (add1 (+ n m))
|#
(claim add1-jumps-out
  (Π ([n Nat]
      [m Nat])
    (= Nat (+ (add1 n) m) (add1 (+ n m)))))
(define add1-jumps-out
  (λ (n m)
    #|
    (= Nat (+ (add1 n) m) (add1 (+ n m)))
    1, find out all eliminator in the expression
    2, if the target of the eliminator has add1 on the top, then the expression
    can be reduced (the eliminator is called a redex)
    (rec-Nat (add1 n)
    m
    (λ (n-1 almost) #|step|#
    (add1 almost)))
    =>
    (step n (rec-Nat n m step))
    (step n (+ n m))
    (add1 (+ n m))
    (= Nat (add1 (+ n m)) (add1 (+ n m)))
    |#
    (same (add1 (+ n m)))))

#|
∀ n,m. (add1 (+ n m)) ≡ (+ (add1 n) m)
|#
(claim add1-jumps-in
  (Π ([n Nat]
      [m Nat])
    (= Nat (add1 (+ n m)) (+ (add1 n) m))))
(define add1-jumps-in
  (λ (n m)
    (ind-Nat n
      (λ (?) (= Nat
                (add1 (+ ? m))
                (+ (add1 ?) m)))
      #|(= Nat (add1 (+ 0 m)) (+ (add1 0) m))|#
      #|(= Nat (add1 m) (+ (add1 0) m))|#
      #|(= Nat (add1 m) (add1 (+ 0 m)))|#
      #|(= Nat (add1 m) (add1 m))|#
      (same (add1 m))
      (λ (k ih)
        #|
        k : Nat
        ih : (= Nat (add1 (+ k m)) (add1 (+ k m)))
        (add1 (+ k m)) = (add1 (+ k m))
        (add1 (add1 (+ k m))) = (add1 (add1 (+ k m)))
        goal : (= Nat (add1 (add1 (+ k m))) (add1 (add1 (+ k m))))
        
        |#
        #|
        congruence : (cong (= X a b) f) ⇒ (= X (f a) (f b))
        |#
        (cong ih (+ 1)
              #;
              (the (→ Nat Nat)
                   (λ (x)
                     (add1 x)))))
      )))
#;
(define add1-jumps-in
  (λ (n m)
    #|(symm (= X a b)) ⇒ (= X b a)|#
    (symm (add1-jumps-out n m))))

(claim +-is-associative
  (Π ([m Nat]
      [n Nat]
      [p Nat])
    (= Nat (+ m (+ n p)) (+ (+ m n) p))))
(define +-is-associative
  (λ (m n p)
    (ind-Nat m
      (λ (?) (= Nat (+ ? (+ n p)) (+ (+ ? n) p)))
      #|(= Nat (+ n p) (+ n p))|#
      (same (+ n p))
      (λ (k ih)
        #|
        k : Nat
        ih : (= Nat (+ k (+ n p)) (+ (+ k n) p))
        goal :  (= Nat (add1 (+ k (+ n p))) (add1 (+ (+ k n) p)))
        |#
        (cong ih (+ 1))))))


(claim Even
  (→ Nat
    U))
(define Even
  (λ (n)
    (Σ ([n/2 Nat])
      (= Nat n (+ n/2 n/2)))))

(claim four-is-Even
  (Even 4))
(define four-is-Even
  (cons 2 (same 4)))
