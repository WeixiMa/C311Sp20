#lang racket
#|miniKanren|#
(require "mk.rkt")
(require "numbers.rkt")

#;
(run 10 (numerator denominator value remainder)
  (/o numerator denominator value remainder))

(define append
  (λ (xs ys)
    (cond
      [(null? xs) ys]
      [else (let ([a (car xs)]
                  [d (cdr xs)])
              (let ([r (append d ys)])
                (cons a r)))])))

(defrel (nullᵒ x)
  (≡ '() x))

#|rewrite the append function to the appendᵒ relation|#
(defrel (appendᵒ xs ys o)
  (condᵉ
   [(nullᵒ xs) (== ys o)]
   [(fresh (a d)
      (== `(,a . ,d) xs)
      (fresh (r)
        (== `(,a . ,r) o)
        (appendᵒ d ys r)))]))

(run 10 (xs ys o)
  (appendᵒ xs ys o))
