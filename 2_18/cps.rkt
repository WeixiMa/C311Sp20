#lang typed/racket
;(require racket/trace)

#|
- context
- factorial
- CPS-rules
-- tail call & serious call
- stack
|#

#|
Serious calls are the functions you defined,
especially the recursive ones.
Simple calls are those from Racket.
A tail call is function calls with no immediate context.
e.g. g in (g x) is a tail call, g in (f (g x)) is not a
tail call.
|#

#|
rules to CPS:
1, change the names, add -cps to every function
2, use lets to move the serious calls to tail position
3, add a k parameter to every lambda
4, rewrite lets using lambdas, note that every function
now takes an extra argument.
5, return the simple values by applying k to it, note that
lambdas are simple
|#

#;
(define fib
  (λ (n)
    (cond
      [(zero? n) 1]
      [(zero? (sub1 n)) 1]
      [else (let ([hole₁ (fib (sub1 n))])
              (let ([hole₂ (fib (sub1 (sub1 n)))])
                (+ hole₁ hole₂)))])))

(: fib-cps (→ Number (→ Number Number)
              Number))
(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [(zero? (sub1 n)) (k 1)]
      [else (fib-cps (sub1 n)
                     (λ (hole₁)
                       (fib-cps (sub1 (sub1 n))
                                (λ (hole₂)
                                  (k (+ hole₁ hole₂))))))])))
#;
(trace fib-cps)
#;
(fib-cps 10 (λ (v) v))

(: plus (→ Number (→ Number Number)))
(define plus
  (λ (m)
    (λ (n)
      (+ m n))))
#;
(let ([f (plus 3)])
  (f 5))

(: plus-cps (→ Number (→ (→ Number (→ Number Number) Number) (→ Number (→ Number Number) Number))
               (→ Number (→ Number Number) Number)))
(define plus-cps
  (λ (m k)
    (k (λ (n k)
         (k (+ m n))))))
#;
(plus-cps 3 (λ (f) (f 5 (λ (v) v))))

(: map-cps (All (A B) (→ (→ A (→ B (Listof B)) (Listof B)) (Listof A) (→ (Listof B) (Listof B))
                         (Listof B))))
(define map-cps
  (λ (f-cps ls k)
    (cond
      [(null? ls) (k '())]
      [else (map-cps f-cps (cdr ls)
                     (λ ([d : (Listof B)])
                       (f-cps (car ls)
                              (λ ([a : B])
                                (k (cons a d))))))]
      #;
      [else (let ([a (f-cps (car ls))]
                  [d (map-cps f-cps (cdr ls))])
              (cons a d))])))
#;
(define add1-cps
  (λ (n k)
    (k (add1 n))))

#;
(map-cps add1-cps '(1 2 3) (λ (v) v))
