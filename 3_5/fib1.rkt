#lang racket
(require "parenthec.rkt")

#|
1, introduce the union for continuations
2, change every place where you build a continuation, now use union
3, change the match that matches a continuation, now it uses union-case
|#

(define-union continuation
  (init)
  (sub1 n k)
  (sub2 k v₁))

#;
(define-types Continuation
  (U (List 'init)
     (List 'sub1 Number Continuation)
     (List 'sub2 Continuation Value)))

#|
(define-union expression
  )
(define-union environment
  )
(define-union closure
  )
|#

(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (apply-k k 1)]
      [(zero? (sub1 n)) (apply-k k 1)]
      [else (fib-cps (sub1 n) (continuation_sub1 n k))])))

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
    #|You need to specify the type of the union in union-case.|#
    (union-case k continuation
      [(init) v]
      [(sub2 k v₁) (apply-k k (+ v₁ v))]
      [(sub1 n k) (fib-cps (sub1 (sub1 n)) (continuation_sub2 k v))])))

(fib-cps 5 (continuation_init))
