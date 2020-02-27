#lang racket

(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (apply-k k 1)]
      [(zero? (sub1 n)) (apply-k k 1)]
      [else (fib-cps (sub1 n) (make-k-sub1 n k))])))

(define make-k-sub1
  (λ (n k)
    (λ (v₁)
      (fib-cps (sub1 (sub1 n)) (make-k-sub2 k v₁)))))

(define make-k-sub2
  (λ (k v₁)
    (λ (v₂)
      (apply-k k (+ v₁ v₂)))))

(define apply-k
  (λ (k v)
    (k v)))

(fib-cps 5 (λ (v) v))
