#lang racket
(require racket/trace)

#|Don't mind this.|#
(define sub2 (compose sub1 sub1))
(define one? (compose zero? sub1))

#|fib-cpsed and ri|#
(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (apply-k k 1)]
      [(one? n) (apply-k k 1)]
      [else (fib-cps (sub1 n)
                     (make-k-sub1 n k))])))

(define make-k-sub1
  (λ (n k)
    `(sub1 ,n ,k)))

(define make-k-sub2
  (λ (k fib-of-sub1)
    `(sub2 ,k ,fib-of-sub1)))

(define make-k-init
  (λ ()
    `(init)))

(define apply-k
  (λ (k v)
    (match k
      [`(sub1 ,n ,k)
       (fib-cps (sub2 n)
                (make-k-sub2 k v))]
      [`(sub2 ,k ,fib-of-sub1)
       (apply-k k (+ fib-of-sub1 v))]
      [`(init) v])))

#;
(trace fib-cps)
(fib-cps 5 (make-k-init))
