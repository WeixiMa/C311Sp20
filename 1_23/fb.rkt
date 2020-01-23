#lang typed/racket/no-check
(require typed/rackunit)

(define-type Exp
  (U Symbol
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))

#|λ-encoding of three|#
(define three
  (λ (step)
    (λ (base)
      (step (step (step base))))))

#|
(λ (x) (y (λ (x) x)))
        y occurs free
                 x occurs bound
|#

(: occurs-free? (→ Symbol Exp
                   Boolean))
(define occurs-free?
  (λ (a e)
    (match e
      [`,y
       #:when (symbol? y)
       (eqv? y a)]
      [`(λ (,x) ,body)
       (and (not (eqv? x a)) (occurs-free? a body))]
      [`(,rator ,rand)
       #|Always start with the last line!|#
       (or (occurs-free? a rator) (occurs-free? a rand))])))

(check-eqv? (occurs-free? 'x 'y) #f)
(check-eqv? (occurs-free? 'x 'x) #t)
(check-eqv? (occurs-free? 'z '(x y)) #f)
(check-eqv? (occurs-free? 'z '(x z)) #t)
(check-eqv? (occurs-free? 'z '((x z) (x y))) #t)
(check-eqv? (occurs-free? 'z '(λ (z) ((x z) (x y)))) #f)

(: occurs-bound? (→ Symbol Exp
                   Boolean))
(define occurs-bound?
  (λ (a e)
    (match e
      [`,y
       #:when (symbol? y)
       #f]
      [`(λ (,x) ,body)
       (or (and (eqv? x a) (occurs-free? a body))
           (occurs-bound? a body))]
      [`(,rator ,rand)
       (or (occurs-bound? a rator) (occurs-bound? a rand))])))

(check-eqv? (occurs-bound? 'x 'y) #f)
(check-eqv? (occurs-bound? 'x 'x) #f)
(check-eqv? (occurs-bound? 'x '(λ (x) (λ (x) x))) #t)
(check-eqv? (occurs-bound? 'z '(λ (z) (x y))) #f)
(check-eqv? (occurs-bound? 'z '(x z)) #f)
(check-eqv? (occurs-bound? 'z '((x z) (x y))) #f)
(check-eqv? (occurs-bound? 'z '(λ (z) ((x z) (x y)))) #t)

#|
de Bruijn indices: a means to compare expressions without worrying about the names
for example:
(λ (a) (λ (b) (a b))) and (λ (x) (λ (y) (x y)))
are two different Exp.
But we can convert them using de Bruijn indices and
(λ (λ (1 0))) and  (λ (λ (1 0)))
are the same DExp.
|#
(define-type DExp
  (U Number         #|variables are now represented by numbers|#
     (List 'λ DExp) #|names are not nessesary here|#
     (List DExp DExp)))


#|
for example
(λ (a)
  (a (λ (b)
       ((λ (a)
          (λ (c)
            (λ (a)
              (λ (d)
                ((a c) d))))) b))))
converts to
(λ 
  (0 (λ
       ((λ
          (λ
            (λ
              (λ
                ((1 2) 0))))) 0))))
|#


