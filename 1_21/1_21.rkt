#lang typed/racket/no-check

#|
- match
|#


;(: some-sentence (Listof Symbol))
(: some-sentence (List 'mary 'had Number 'little 'lamb))
(define some-sentence
  '(mary had 1 little lamb))
#|
'(mary had a little lamb) is the short hand for
(cons 'mary (cons 'had (cons 'a (cons 'little (cons 'lamb '())))))
|#

#|
quasiquote (`) is like quote (') -- named by Willard Van Orman Quine
except that you can use commas (,) inside the sentence
commas introduce variables
|#

#;
((λ (x)
    (match x
      [`(,person had ,n little ,animal) `(,person ,n ,animal)]
      [whatever 'nothing-matched]))
 some-sentence)

#|
λ-calculus:
a λ-expression is one of the following.
- y             if y is a symbol
- (λ (x) body)  if x is symbol and body is a λ-expression
- (rator rand)  if rator and rand are λ-expressions
|#

(define-type Exp
  (U Symbol
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))

#|tabNine|#

(: exp? (→ Any Boolean))
(define exp?
  (λ (x)
    (match x
      [`,y
       #:when (symbol? y)
       #t]
      [`(λ (,x) ,body)
       (and (symbol? x) (exp? body))]
      [`(,rator ,rand)
       (and (exp? rator) (exp? rand))]
      [`,whatever
       #f])
    #;
    #|Something's not quite right here.|#
    (cond
      [(symbol? x) #t]
      [(and (pair? x)
            (eqv? (car x) 'λ)
            (and (pair? (car (cdr x)))
                 (symbol? (car (car (cdr x))))
                 (null? (cdr (car (cdr x)))))
            (exp? (car (cdr (cdr x))))
            (null? (cdr (cdr (cdr x)))))
       #t]
      [(and (pair? x)
            (exp? (car x))
            (exp? (car (cdr x)))
            (null? (cdr (cdr x))))
       #t]
      [else
       #f])))


#;
(exp? `((λ (x) (x x)) (λ (x) (x x))))

#|
L0
- Number
- `(pluz L0 L0)
- `(monus L0 L0)
|#
(define-type L0
  (U Number
     (List 'pluz L0 L0)
     (List 'monus L0 L0)))

(: L0? (→ Any Boolean))
(define L0?
  (λ (x)
    (match x
      [`,n
       #:when (number? n)
       #t]
      [`(pluz ,e₁ ,e₂)
       (and (L0? e₁) (L0? e₂))]
      [`(monus ,e₁ ,e₂)
       (and (L0? e₁) (L0? e₂))]
      [`,whatever
       #f])))

(: valof (→ L0 Number))
(define valof
  (λ (e)
    (match e
      [`,n
       #:when (number? n)
       n]
      [`(pluz ,e₁ ,e₂)
       (+ (valof e₁) (valof e₂))]
      [`(monus ,e₁ ,e₂)
       (- (valof e₁) (valof e₂))])))

(valof '(monus (pluz (pluz 3 5) 10) 8))
