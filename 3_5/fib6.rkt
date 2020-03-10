#lang racket
(require "parenthec.rkt")

#|
Change define thunks to define labels.
|#

(define-registers fib-cps-n apply-k-v cc)

(define-union continuation
  (init)
  (sub1 n k)
  (sub2 k v₁))

(define-label fib-cps
  (cond
    [(zero? fib-cps-n)
     (begin [set! cc cc]
            [set! apply-k-v 1]
            (apply-k))]
    [(zero? (sub1 fib-cps-n))
     (begin [set! cc cc]
            [set! apply-k-v 1]
            (apply-k))]
    [else (begin [set! cc (continuation_sub1 fib-cps-n cc)]
                 [set! fib-cps-n (sub1 fib-cps-n)]
                 (fib-cps))]))

(define-label apply-k
  (union-case cc continuation
    [(init) apply-k-v]
    [(sub2 k v₁)
     (begin [set! cc k]
            [set! apply-k-v (+ v₁ apply-k-v)]
            (apply-k))]
    [(sub1 n k)
     (begin [set! cc (continuation_sub2 k apply-k-v)]
            [set! fib-cps-n (sub1 (sub1 n))]
            (fib-cps))]))

(begin [set! cc (continuation_init)]
       [set! fib-cps-n 5]
       (fib-cps))
