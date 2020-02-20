#lang racket
(require racket/trace)

(define !-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [else (!-cps (sub1 n)
                   (λ (!-of-sub1)
                     (k (* n !-of-sub1))))])))

(define fib
  (λ (n)
    (cond
      [(zero? n) 1]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 n))
               (fib (sub1 (sub1 n))))])))

(trace fib)
#;
(fib 10)

(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [(zero? (sub1 n)) (k 1)]
      [else (fib-cps (sub1 n)
                     (λ (fib-of-sub1)
                       (fib-cps (sub1 (sub1 n))
                                (λ (fib-of-sub2)
                                  (k (+ fib-of-sub1 fib-of-sub2))))))])))

(trace fib-cps)
#;
(fib-cps 10 (λ (v) v))





#|
In a cps-ed function, all serious calls have to be in tail positions.
|#

(define valof-cps
  (λ (exp env k)
    (match exp
      [`,y
       #:when (symbol? y)
       (k (env y))]
      [`,n
       #:when (number? n)
       (k n)]
      [`(+ ,e₁ ,e₂)
       (valof-cps e₁ env
                  (λ (n₁)
                    (valof-cps e₂ env
                               (λ (n₂)
                                 (k (+ n₁ n₂))))))]
      #|(let/cc kvar body) is like (let ([kvar k]) body) where k is the current continuation|#
      [`(let/cc ,kvar ,body)
       (valof-cps body (λ (y) (if (eqv? y kvar) k (env y))) k)]
      [`(throw ,ke ,ve)
       (valof-cps ke env
                  (λ (k^)
                    (valof-cps ve env
                               k^
                               #;
                               (λ (v)
                                 (k^ v)))))]
      [`(λ (,x) ,body)
       #|this is the short-hand of the following|#
       (k (λ (a k^) (valof-cps body (λ (y) (if (eqv? y x) a (env y)))
                               k^)))
       #;
       (k (λ (a k^) (valof-cps body (λ (y) (if (eqv? y x) a (env y)))
                               (λ (v) (k^ v)))))]
      [`(,rator ,rand)
       (valof-cps rator env
                  (λ (closure)
                    (valof-cps rand env
                               (λ (a)
                                 #|this is the short-hand of the following|#
                                 (closure a k)
                                 #;
                                 (closure a (λ (v) (k v)))))))])))

(valof-cps '(let/cc k (((λ (x) (λ (y) (+ y (throw k (+ (throw k y) x))))) 5) 10))
           (λ (y) (error "unbound " y))
           (λ (v) v))
