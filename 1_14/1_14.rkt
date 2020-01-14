#lang typed/racket

(: + (→ Number Number
        Number))
(define +
  (λ (n m)
    (cond
      [(zero? n) m]
      #|(+ (sub1 n) m) is the natural recursion
        + recurs on n, because (zero? n) terminates recursion
        so at each step, (sub1 n) makes recursion closer to termination.
      |#
      #|(add1 ..) is a wrapper that takes the result of natural recursion
        to the result we want|#
      [else (add1 (+ (sub1 n) m))])))

#;
(+ 17 5)

(: * (→ Number Number
        Number))
(define *
  (λ (n m)
    (cond
      [(zero? n) 0]
      [else (+ m (* (sub1 n) m))])))

(: ↑ (→ Number Number
        Number))
(define ↑
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (* n (↑ n (sub1 m)))])))

#;
(↑ 3 5)

(: count-occurs (All (A) (→ A (Listof A)
                            Number)))
(define count-occurs
  (λ (x ls)
    (cond
      [(null? ls) 0]
      #|car takes the first one from the list|#
      #|cdr takes everything but the first one from the list|#
      [(eqv? (car ls) x) (add1 (count-occurs x (cdr ls)))]
      [else (count-occurs x (cdr ls))])))

#;
(count-occurs 'b '(a b c b))
#;
(count-occurs 42 '(1 2 42 42 3))

(: member? (All (A) (→ A (Listof A)
                       Boolean)))
(define member?
  (λ (x ls)
    (cond
      [(null? ls) #f]
      [else (or (eqv? (car ls) x) (member? x (cdr ls)))])))

#;
(member? 'cat '(cat dog dog))

#;
(define count-occurs*
  (λ (x ls)
    (cond
      [(null? ls) 0]
      [(pair? (car ls)) (+ (count-occurs* x (car ls))
                           (count-occurs* x (cdr ls)))]
      [(eqv? (car ls) x) (add1 (count-occurs* x (cdr ls)))]
      [else (count-occurs* x (cdr ls))])))

#;
(count-occurs 42 '(1 2 42 42 42 3))
