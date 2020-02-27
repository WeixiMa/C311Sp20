#lang racket

(define fib-k #f)
(define fib-n #f)
(define apply-k-k #f)
(define apply-k-v #f)

(define fib-cps
  (λ (fib-n fib-k)
    (cond
      [(zero? fib-n) (let* ([apply-k-k fib-k]
                            [apply-k-v 1])
                       (apply-k apply-k-k apply-k-v))]
      [(zero? (sub1 fib-n)) (let* ([apply-k-k fib-k]
                                   [apply-k-v 1])
                              (apply-k apply-k-k apply-k-v))]
      [else (let* ([fib-k (make-k-sub1 fib-n fib-k)]
                   [fib-n (sub1 fib-n)])
              (fib-cps fib-n fib-k))])))

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
