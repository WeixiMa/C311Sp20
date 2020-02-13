#lang typed/racket

(: memv (All (A) (→ A (Listof A) (U (Listof A) False))))
(define memv
  (λ (a as)
    (match as
      ['() #f]
      [`(,a^ . ,d)
       (if (eqv? a^ a)
                as
                (memv a d))])
    #;
    (cond
      [(null? as) #f]
      [else (if (eqv? (car as) a)
                as
                (memv a (cdr as)))])))

#;
(memv 'x '(a a y z))

(let ([a 5]
      [b 6])
  `(a b (+ a b)))


(define minus
  (lambda (n1 n2)
    (cond
      [(zero? n2) n1]
      [else (sub1
             (minus n1 (sub1 n2)))])))
