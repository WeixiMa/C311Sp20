#lang racket

(define fib-cps
  (λ (n cc)
    (cond
      [(zero? n) (let* ([k cc]
                        [v 1])
                   (apply-k k v))]
      [(zero? (sub1 n)) (let* ([k cc]
                               [v 1])
                          (apply-k k v))]
      [else (let* ([k (make-k-sub1 n cc)]
                   [n (sub1 n)])
              (fib-cps n k))])))

(define make-k-sub1
  (λ (n k)
    `(sub1 ,n ,k)))

(define make-k-sub2
  (λ (k v₁)
    `(sub2 ,k ,v₁)))

(define make-k-init
  (λ ()
    '(init)))

(define apply-k
  (λ (k v)
    (match k
      ['(init) v]
      [`(sub2 ,k ,v₁) (apply-k k (+ v₁ v))]
      [`(sub1 ,n ,k) (fib-cps (sub1 (sub1 n)) (make-k-sub2 k v))])))

(fib-cps 5 (make-k-init))
