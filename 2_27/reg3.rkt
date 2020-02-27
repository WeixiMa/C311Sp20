#lang racket

(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (apply-k k 1)]
      [(zero? (sub1 n)) (apply-k k 1)]
      [else (fib-cps (sub1 n) (make-k-sub1 n k))])))

(define make-k-sub1
  (λ (n k)
    `(sub1 ,n ,k)
    #;
    (λ (v)
      (fib-cps (sub1 (sub1 n)) (make-k-sub2 k v)))))

(define make-k-sub2
  (λ (k v₁)
    `(sub2 ,k ,v₁)
    #;
    (λ (v)
      (apply-k k (+ v₁ v)))))

(define make-k-init
  (λ ()
    '(init)
    #;
    (λ (v) v)))

(define apply-k
  (λ (k v)
    (match k
      ['(init) v]
      [`(sub2 ,k ,v₁) (apply-k k (+ v₁ v))]
      [`(sub1 ,n ,k) (fib-cps (sub1 (sub1 n)) (make-k-sub2 k v))])))

(fib-cps 5 (make-k-init))
