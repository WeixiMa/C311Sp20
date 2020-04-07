#lang racket
(require "monads.rkt")

(define store '())
(define fib/effects
  (λ (n)
    (cond
      [(assv n store)
       =>
       (λ (assv-result) (cdr assv-result))]
      [(zero? n) 1]
      [(zero? (sub1 n)) 1]
      [else (let ([fib-sub1 (fib/effects (sub1 n))]
                  [fib-sub2 (fib/effects (sub1 (sub1 n)))])
              (begin (set! store `((,n . ,(+ fib-sub1 fib-sub2)) . ,store))
                     (+ fib-sub1 fib-sub2)))])))

(define fib/sps
  (λ (n store)
    (cond
      [(assv n store)
       =>
       (λ (assv-result) `(,(cdr assv-result) . ,store))]
      [(zero? n) `(1 . ,store)]
      [(zero? (sub1 n)) `(1 . ,store)]
      [else (match-let* ([`(,fib-sub1 . ,store) (fib/sps (sub1 n) store)]
                         [`(,fib-sub2 . ,store) (fib/sps (sub1 (sub1 n)) store)])
              `(,(+ fib-sub1 fib-sub2) . ((,n . ,(+ fib-sub1 fib-sub2)) . ,store)))])))

#|
A programming language is low level when its programs require attention to the irrelevant.
-- Alan Perlis
|#
#;
(fib/sps 100 '())

#|State monad|#
#|
state monad : is a pair of a pure and a store
inj-state : puts something at the pure part (→ A (State S A))
put : puts something at the store part (→ S (State S A))
bind-state : (→ (State S A) (→ A (State S B)) (State S B))
get : takes the store part from a state monad (→ (State S A) S)
(get `(,store . ,pure)) => `(,store . ,store)
|#

#;
((run-state (inj-state 5)) 'init-state)
#;
((run-state (put 'new-state)) 'init-state)
#;
((run-state
   (bind-state (inj-state 5)
               (λ (pure)
                 (inj-state (zero? pure)))))
 'dc)
#;
((run-state
   (bind-state (put 5)
               (λ (pure)
                 (get))))
 'dc)


; (→ Number (State (Listof (Pairof Number Number)) Number))
#;
(define fib
  (λ (n)
    (bind-state (get)
                (λ (store)
                  (cond
                    [(assv n store)
                     =>
                     (λ (assv-result)
                       (inj-state (cdr assv-result)))]
                    [(zero? n) (inj-state 1)]
                    [(zero? (sub1 n)) (inj-state 1)]
                    [else (bind-state (fib (sub1 n))
                                      (λ (fib-sub1)
                                        (bind-state (fib (sub1 (sub1 n)))
                                                    (λ (fib-sub2)
                                                      (bind-state (get)
                                                                  (λ (store)
                                                                    (bind-state (put `((,n . ,(+ fib-sub1 fib-sub2)) . ,store))
                                                                                (λ (_)
                                                                                  (inj-state (+ fib-sub1 fib-sub2))))))))))])))))

#;
(cdr ((run-state (fib 1000)) '()))

#;
((run-state
   (bind-state (inj-state 5)
               (λ (pure)
                 (bind-state (inj-state (zero? pure))
                             (λ (bool)
                               (inj-state (eqv? #t bool)))))))
 'dc)

#;
((run-state
   (go-on ([pure (inj-state 5)]
           [bool (inj-state (zero? pure))])
     (inj-state (eqv? #t bool))))
 'dc)

(define fib
  (λ (n)
    (bind-state (get)
                (λ (store)
                  (cond
                    [(assv n store)
                     =>
                     (λ (assv-result)
                       (inj-state (cdr assv-result)))]
                    [(zero? n) (inj-state 1)]
                    [(zero? (sub1 n)) (inj-state 1)]
                    [else
                     (go-on ([fib-sub1 (fib (sub1 n))]
                             [fib-sub2 (fib (sub1 (sub1 n)))]
                             [store (get)]
                             [_ (put `((,n . ,(+ fib-sub1 fib-sub2)) . ,store))])
                       (inj-state (+ fib-sub1 fib-sub2)))])))))

#;
(cdr ((run-state (fib 1000)) '()))

#|
maybe monad : is a pair of a pure and a store
Just : puts something at the pure part (→ A (Maybe A))
Nothing : puts something at the store part (→ (Maybe A))
bind-maybe: (→ (Maybe A) (→ A (Maybe B)) (Maybe B))
|#
; (: lookup (→ Symbol (Listof (Pairof Symbol A)) (Maybe A)))
(define lookup
  (λ (x ls)
    (match ls
      ['() (Nothing)]
      [`((,x^ . ,v) . ,ls^)
       (if (eqv? x^ x)
           (Just v)
           (lookup x ls^))])))

#;
(lookup 'x '((x . (Nothing)) (y . 10)))
