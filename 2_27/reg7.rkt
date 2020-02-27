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
  (λ (apply-k-k apply-k-v)
    (match apply-k-k
      ['(init) apply-k-v]
      [`(sub2 ,k ,v₁) (let* ([apply-k-k k]
                             [apply-k-v (+ v₁ apply-k-v)])
                        (apply-k apply-k-k apply-k-v))]
      [`(sub1 ,n ,k)
       (let* ([fib-k (make-k-sub2 k apply-k-v)]
              [fib-n (sub1 (sub1 n))])
         (fib-cps fib-n fib-k))])))

(let* ([fib-k (make-k-init)]
       [fib-n 5])
  (fib-cps fib-n fib-k))
