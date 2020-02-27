#lang racket

#|fib in A-normal form|#

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
      [`(sub2 ,k ,v₁) (let* ([k k]
                             [v (+ v₁ v)])
                        (apply-k k v))]
      [`(sub1 ,n ,k)
       (let* ([k (make-k-sub2 k v)]
              [n (sub1 (sub1 n))])
         (fib-cps n k))])))

(let* ([k (make-k-init)]
       [n 5])
  (fib-cps n k))
