#lang racket
(require racket/trace)

(define fib-k #f)
(define fib-n #f)
(define apply-k-k #f)
(define apply-k-v #f)

(define fib-cps
  (λ () ; fib-n fib-k
    (cond
      [(zero? fib-n) (begin [set! apply-k-k fib-k]
                            [set! apply-k-v 1]
                            (apply-k))]
      [(zero? (sub1 fib-n)) (begin [set! apply-k-k fib-k]
                                   [set! apply-k-v 1]
                                   (apply-k))]
      [else (begin [set! fib-k (make-k-sub1 fib-n fib-k)]
                   [set! fib-n (sub1 fib-n)]
                   (fib-cps))])))
(trace fib-cps)

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
  (λ () ;apply-k-k apply-k-v
    (match apply-k-k
      ['(init) apply-k-v]
      [`(sub2 ,k ,v₁) (begin [set! apply-k-k k]
                             [set! apply-k-v (+ v₁ apply-k-v)]
                             (apply-k))]
      [`(sub1 ,n ,k)
       (begin [set! fib-k (make-k-sub2 k apply-k-v)]
              [set! fib-n (sub1 (sub1 n))]
              (fib-cps))])))
(trace apply-k)


(begin [set! fib-k (make-k-init)]
       [set! fib-n 5]
       (fib-cps))
