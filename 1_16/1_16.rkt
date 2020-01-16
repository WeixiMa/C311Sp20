#lang typed/racket

#||#

#;
(: + (→ Number Number Number))
#;
(define +
  (λ (n m)
    (cond
      [(zero? m) n]
      [else (add1 (+ n (sub1 m)))])))

#;
(: * (→ Number Number Number))
#;
(define *
  (λ (n m)
    (cond
      [(zero? m) 0]
      [else (+ n (* n (sub1 m)))])))

#;
(: ↑ (→ Number Number Number))
#;
(define ↑
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (* n (↑ n (sub1 m)))])))

#;
(: ⇑ (→ Number Number Number))
#;
(define ⇑
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (↑ n (⇑ n (sub1 m)))])))

#|We are gonna use higher-order functions(functions whose inputs and outputs
  can be other functions) to generate an arrow with 100 hats|#
#|G takes an index(number) and spits out a function that looks like one of
  the above|#

#|a variation of the Ackermann function,
the first known general recursion function|#
(: G (→ Number (→ Number Number Number)))
(define G
  (λ (i)
    (λ (n m)
      (cond
        [(zero? i)
         (cond
           [(zero? m) n]
           [else (add1 ((G 0) n (sub1 m)))])]
        [(zero? m)
         (cond
           [(zero? (sub1 i)) 0]
           [else 1])]
        [else
         ((G (sub1 i)) n ((G i) n (sub1 m)))]))))

#|higher-order functions are not really needed here|#
#;
(: G (→ Number Number Number Number))
#;
(define G
  (λ (i n m)
    (cond
      [(zero? i)
       (cond
         [(zero? m) n]
         [else (add1 (G 0 n (sub1 m)))])]
      [(zero? m)
       (cond
         [(zero? (sub1 i)) 0]
         [else 1])]
      [else
       (G (sub1 i) n (G i n (sub1 m)))])))

#|the Ackermann function|#
(: A (→ Number Number Number))
(define A
  (λ (m n)
    (cond
      [(zero? m)
       (add1 n)]
      [(zero? n)
       (A (sub1 m) 1)]
      [else
       (A (sub1 m) (A m (sub1 n)))])))
