#lang racket
(require racket/trace)

#|Don't mind this.|#
(define sub2 (compose sub1 sub1))
(define one? (compose zero? sub1))

#|cpsed fib|#
(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [(one? n) (k 1)]
      [else (fib-cps (sub1 n)
                     (λ (fib-of-sub1)
                       (fib-cps (sub2 n)
                                (λ (fib-of-sub2)
                                  (k (+ fib-of-sub1 fib-of-sub2))))))])))

(trace fib-cps)
(fib-cps 5 (λ (v) v))
