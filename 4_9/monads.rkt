#lang typed/racket/no-check
;#lang typed/racket

(provide (all-defined-out))

(struct (A) Just
  ([a : A])
  #:transparent)

(struct Nothing
  ()
  #:transparent)

(define-type (Maybe A)
  (U (Just A)
     Nothing))

(: bind-maybe (All (A B) (-> (Maybe A) (-> A (Maybe B)) (Maybe B))))
(define (bind-maybe ma f)
  (match ma
    [(Just a) (f a)]
    [(Nothing) (Nothing)]))

(struct Unit
  ())

(struct (W A) Writer
  ([log : (Listof W)]
   [a : A]))

(: run-writer (All (W A) (-> (Writer W A) (Pair (Listof W) A))))
(define (run-writer ma)
  (match ma
    [(Writer log a) (cons log a)]))

(: inj-writer (All (W A) (-> A (Writer W A))))
(define (inj-writer a)
  (Writer '() a))

(: tell (All (W) (-> W (Writer W Unit))))
(define (tell msg)
  (Writer (list msg) (Unit)))

(: bind-writer (All (W A B) (-> (Writer W A) (-> A (Writer W B)) (Writer W B))))
(define (bind-writer ma f)
  (match ma
    [(Writer la a) (match (run-writer (f a))
                     [(cons lb b) (Writer (append la lb) b)])]))

(: pass (All (W A) (-> (Writer W A) (-> (Listof W) (Listof W)) (Writer W A))))
(define (pass ma f)
  (match ma
    [(Writer la a) (Writer (f la) a)]))

(: listen (All (W A) (-> (Writer W A) (Writer W (Pair (Listof W) A)))))
(define (listen ma)
  (match ma
    [(Writer la a) (Writer la (cons la a))]))

(struct (Store A) State
  ([run-state : (-> Store (Pair Store A))]))

(: inj-state (All (S A) (-> A (State S A))))
(define (inj-state a)
  (State (λ ([s : S]) (cons s a))))

(: run-state (All (S A) (-> (State S A) (-> S (Pair S A)))))
(define run-state State-run-state)

(: bind-state (All (S A B) (-> (State S A)  (-> A (State S B)) (State S B))))
(define (bind-state ma f)
  (State (λ ([s : S])
           (match ((run-state ma) s)
             [`(,s . ,a) ((run-state (f a)) s)]))))

(: get (All (S) (-> (State S S))))
(define (get)
  (State (λ ([s : S])
           (cons s s))))

(: put (All (S) (-> S (State S Unit))))
(define (put s)
  (State (λ (_) (cons s (Unit)))))

(struct (R A) Cont
  ([run-k : (-> (-> A R) R)]))

(: run-k (All (R A) (-> (Cont R A) (-> (-> A R) R))))
(define run-k Cont-run-k)

(: inj-k (All (R A) (-> A (Cont R A))))
(define (inj-k a)
  (Cont (λ ([k : (-> A R)]) (k a))))

(: bind-k (All (A B R) (-> (Cont R A) (-> A (Cont R B)) (Cont R B))))
(define (bind-k ma f)
  (Cont (λ ([k : (-> B R)])
          ((run-k ma) (λ ([a : A]) ((run-k (f a)) k))))))

(: callcc (All (R A B) (-> (-> (-> A (Cont R B)) (Cont R A)) (Cont R A))))
(define (callcc f)
  (Cont (λ ([k : (-> A R)])
          ((run-k (f (λ (a) (Cont (λ (_) (k a))))))
           k))))

(: inj-list (All (A) (Listof A)))
(define (inj-list a)
  `(,a))

(: empty-list (All (A) (Listof A)))
(define (empty-list)
  '())

(: bind-list (All (A B) (→ (Listof A) (→ A (Listof B))
                           (Listof B))))
(define (bind-list ls f)
  (append-map f ls))

(define bind
  (λ (m f)
    (cond
      [(or (Just? m) (Nothing? m)) (bind-maybe m f)]
      [(Writer? m) (bind-writer m f)]
      [(State? m) (bind-state m f)]
      [(Cont? m) (bind-k m f)]
      [(list? m) (bind-list m f)]
      [else (error "bind: ~a is not a monad" m)])))

#;
(define-syntax go-on
  (syntax-rules (<-)
    [(_ e) e]
    [(_ (v₀ <- e₀) e ...)
     (bind e₀ (λ (v₀) (go-on e ...)))]
    [(_ e₀ e ...)
     (bind e₀ (λ (_) (go-on e ...)))]))

(define-syntax go-on
  (syntax-rules ()
    [(_ () b) b]
    [(_ ([v₀ e₀] pr ...) b)
     (bind e₀ (λ (v₀) (go-on (pr ...) b)))]))



