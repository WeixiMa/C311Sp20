#lang racket
(require "parenthec.rkt")

#|
Use let* to make all serious functions in A-normal form.
|#

(define-union continuation
  (init)
  (sub1 n k)
  (sub2 k v₁))

(define fib-cps
  (λ (n k)
    (cond
      [(zero? n)
       (let* ([k k]
              [v 1])
         (apply-k k v))]
      [(zero? (sub1 n))
       (let* ([k k]
              [v 1])
         (apply-k k v))]
      [else (let* ([k (continuation_sub1 n k)]
                   [n (sub1 n)])
              (fib-cps n k))])))

(define apply-k
  (λ (k v)
    #|You need to specify the type of the union in union-case.|#
    (union-case k continuation
      [(init) v]
      [(sub2 k v₁)
       (let* ([k k]
              [v (+ v₁ v)])
         (apply-k k v))]
      [(sub1 n k)
       (let* ([k (continuation_sub2 k v)]
              [n (sub1 (sub1 n))])
         (fib-cps n k))])))

(let* ([k (continuation_init)]
       [n 5])
  (fib-cps n k))
