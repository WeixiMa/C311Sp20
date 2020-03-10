#lang racket
(require "parenthec.rkt")

#|
Registerize all serious functions.
|#

(define-registers fib-cps-n apply-k-v cc)

(define-union continuation
  (init)
  (sub1 n k)
  (sub2 k v₁))

(define fib-cps
  (λ (fib-cps-n cc)
    (cond
      [(zero? fib-cps-n)
       (let* ([cc cc]
              [apply-k-v 1])
         (apply-k cc apply-k-v))]
      [(zero? (sub1 fib-cps-n))
       (let* ([cc cc]
              [apply-k-v 1])
         (apply-k cc apply-k-v))]
      [else (let* ([cc (continuation_sub1 fib-cps-n cc)]
                   [fib-cps-n (sub1 fib-cps-n)])
              (fib-cps fib-cps-n cc))])))

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
