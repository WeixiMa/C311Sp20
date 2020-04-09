#lang racket
(require "monads.rkt")

#|
| inject-pure                                | inject-effect                       | bind
state   | inj-state   (→ A (State S A))              | put      (-> S (State S A))         | bind-state  (→ (State S A) (→ A (State S B)) (State S B))
maybe   | Just        (→ A (Maybe A))                | Nothing  (→ (Maybe A))              | bind-maybe  (→ (Maybe A) (→ A (Maybe B)) (Maybe B))
writer  | inj-writer                | tell | bind-writer
cont    | inj-k | callcc | bind-k
list    | (→ A (Listof A)) | (→ (Listof A)) | (→ (Listof A) (→ A (Listof B)) (Listof B)))
|#

#|
For list, inject-pure is (λ (a) (cons a '()))
inject-effect is (λ (_) '())
bind is append-map


to prove law1, we have to show that
(append-map `(,a) f) ≡ (f a)
lhs: (append (f a) (append-map '() f))
lhs: (append (f a) '())
lhs: (f a)

to prove law2, we have to show that
(append-map ls (λ (a) (cons a '()))) ≡ ls
- (append-map '() (λ (a) (cons a '()))) ≡ '()
  '()

- (append-map `(,a . ,d) (λ (a) (cons a '()))) ≡ `(,a . ,d)
(append `(,a) (append-map d f)) ≡ `(,a . ,d)
by induction hypothesis, (append-map d f) ≡ d
lhs: (append `(,a) d)
lhs: `(,a . ,d)


|#

(define product
  (λ (xs ys)
    (bind-list xs
               (λ (x)
                 (bind-list ys
                            (λ (y)
                              (inj-list `(,x . ,y))))))
    #;
    (go-on ([x xs]
            [y ys])
      (inj-list `(,x . ,y)))))
#;
(product '(1 2) '(3 4 5))

(define append-map
  (λ (ls f)
    (cond
      [(null? ls) '()]
      [else (append (f (car ls))
                    (append-map (car ls) f))])))

#|
To claim something T is a monad
find inject-pure, inject-effect, and bind
law1: left-id (bind (inj-pure a) f) ≡ (f a)
law2: right-id (bind ma inj-pure) ≡ ma
law3: assoc (bind (bind ma f) g) ≡ (bind ma (λ (x) (bind (f x) g)))
|#



#;
(run-writer
 (bind-writer (tell 'something-else)
              (λ (_)
                (bind-writer (tell 'something)
                             (λ (_)
                               (inj-writer 5))))))

#;
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`,n
       #:when (number? n)
       n]
      [`(+ ,e₁ ,e₂)
       (+ (valof e₁ env) (valof e₂ env))]
      [`(λ (,x) ,body)
       (λ (a) (valof body (λ (y) (if (eqv? y x) a (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))


#;
(define valof
  (λ (exp env)
    (bind-writer (tell `(valof ,exp ,env))
                 (λ (_)
                   (match exp
                     [`,y
                      #:when (symbol? y)
                      (inj-writer (env y))]
                     [`,n
                      #:when (number? n)
                      (inj-writer n)]
                     [`(+ ,e₁ ,e₂)
                      (go-on ([n₁ (valof e₁ env)]
                              [n₂ (valof e₂ env)])
                        (inj-writer (+ n₁ n₂)))]
                     [`(λ (,x) ,body)
                      (inj-writer (λ (arg) (valof body (λ (y) (if (eqv? y x) arg (env y))))))]
                     [`(,rator ,rand)
                      (go-on ([clos (valof rator env)]
                              [arg (valof rand env)])
                        (clos arg))])))))

#;
(run-writer
 (valof '(((λ (x) (λ (y) (+ x y))) 5) 42)
        (λ (y) (error "unbound " y))))


(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (inj-k (env y))]
      [`,n
       #:when (number? n)
       (inj-k n)]
      [`(+ ,e₁ ,e₂)
       (go-on ([n₁ (valof e₁ env)]
               [n₂ (valof e₂ env)])
         (inj-k (+ n₁ n₂)))]
      [`(let/cc ,kvar ,body)
       #|callcc is call-with-current-continuation|#
       #|this is not Racket's call/cc|#
       #|this is the function in line 101 in monads.rkt|#
       (callcc (λ (k) (valof body (λ (y) (if (eqv? y kvar) k (env y))))))]
      [`(λ (,x) ,body)
       (inj-k (λ (a) (valof body (λ (y) (if (eqv? y x) a (env y))))))]
      [`(,rator ,rand)
       (go-on ([clos (valof rator env)]
               [arg (valof rand env)])
         (clos arg))])))

#;
((run-k
  (valof '(let/cc k (((λ (x) (λ (y) (+ (k x) y))) 5) 42))
         (λ (y) (error "unbound " y))))
 (λ (v) v))
