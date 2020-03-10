#lang racket

(provide defrel
         conde conda condu
         condᵉ
         fresh
         run run*
         == ≡ =/= ≠ absento numbero symbolo numberᵒ symbolᵒ absentᵒ
         succeed fail
         prt)

#|microKanren, Ch 10|#

(define nothing
  (vector))

(define (var sym scp)
  (vector sym scp nothing))
(define var? vector?)
(define (var-has-val? x)
  (not (equal? nothing (vector-ref x 2))))
(define (var-val x)
  (vector-ref x 2))
(define (set-var-val! x v)
  (vector-set! x 2 v))
(define (var-scp x)
  (vector-ref x 1))

#;
(struct State
  (s
   scp
   t
   neqs
   abs))

(struct Scope
  ())

(define State
  (λ (s scp t neqs abs)
    `(,s ,scp ,t ,neqs ,abs)))

(define init-S
  (State (make-immutable-hasheqv) (Scope) (make-immutable-hasheqv) '()
         (make-immutable-hasheqv)))

(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(var-has-val? v)
        (walk (var-val v) s)]
       [(hash-ref s v #f)
        =>
        (λ (v) (walk v s))]
       [else v])]
    [else v]))

(define (ext-s x v s)
  (cond
    [(occurs? x v s) #f]
    [else (hash-set s x v)]))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eqv? v x)]
      [(pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s))]
      [else #f])))

(define (unify u v s scp new-pairs)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(eqv? u v) (cons s new-pairs)]
      [(var? u) (cond
                  [(eqv? (var-scp u) scp)
                   (if (occurs? u v s)
                       #f
                       (begin (set-var-val! u v)
                              (cons s `((,u . ,v) . ,new-pairs))))]
                  [else
                   (go-on ([s (ext-s u v s)])
                     (cons s `((,u . ,v) . ,new-pairs)))])]
      [(var? v) (cond
                  [(eqv? (var-scp v) scp)
                   (if (occurs? v u s)
                       #f
                       (begin (set-var-val! v u)
                              (cons s `((,v . ,u) . ,new-pairs))))]
                  [else
                   (go-on ([s (ext-s v u s)])
                     (cons s `((,v . ,u) . ,new-pairs)))])]
      [(and (pair? u) (pair? v))
       (go-on ([`(,s . ,new-pairs) (unify (car u) (car v) s scp new-pairs)]
               [`(,s . ,new-pairs) (unify (cdr u) (cdr v) s scp new-pairs)])
         (cons s new-pairs))]
      [else #f])))



(define (== u v)
  (λS (S @ s scp t neqs abs)
    (go-on ( [`(,s . ,new-pairs) (unify u v s scp '())]
              [neqs (validate-neqs neqs s)]
              [t (validate-types new-pairs t)]
              [`(,neqs . ,abs) (validate-abs new-pairs neqs abs s)])
        `(,(State s scp t neqs abs))
        '())))

(define (succeed S) `(,S))

(define (fail S) '())

(define ((disj₂ g₁ g₂) S)
  ($append (g₁ S) (g₂ S)))

(define ($append $₁ $₂)
  (cond
    [(null? $₁) $₂]
    [(pair? $₁) (cons (car $₁) ($append (cdr $₁) $₂))]
    [else (λ () ($append $₂ ($₁)))]))

(define ($take n $)
  (cond
    [(and n (zero? n)) '()]
    [(null? $) '()]
    [(pair? $) (cons (car $) ($take (and n (sub1 n)) (cdr $)))]
    [else ($take n ($))]))

(define ((conj₂ g₁ g₂) S)
  ($append-map g₂ (g₁ S)))

(define ($append-map g $)
  (cond
    [(null? $) '()]
    [(pair? $) ($append (g (car $)) ($append-map g (cdr $)))]
    [else (λ () ($append-map g ($)))]))

(define call/fresh
  (λ (name f)
    (λS (S @ s scp t neqs abs)
        ((f (var name scp)) S))))

(define (reify-name n)
  (string->symbol (string-append "_" (number->string n))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [else v])))

(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (let ([n (hash-count s)])
                  (let ([rn (reify-name n)])
                    (hash-set s v rn)))]
      [(pair? v) (let ([s (reify-s (car v) s)])
                   (reify-s (cdr v) s))]
      [else s])))

(define (reify v) 
  (λS (S @ s scp t neqs abs)
      (let ([v (walk* v s)])
        (let ([names (reify-s v (make-immutable-hasheqv))])
          (walk* v names)))))

(define (run-goal n g)
  ($take n (g init-S)))

(define ((ifte g₁ g₂ g₃) S)
  (let loop ([$ (g₁ S)])
    (cond
      [(null? $) (g₃ S)]
      [(pair? $)
       ($append-map g₂ $)]
      [else (λ () (loop ($)))])))

(define ((once g) S)
  (let loop ([$ (g S)])
    (cond
      [(null? $) '()]
      [(pair? $) (cons (car $) '())]
      [else (λ () (loop ($)))])))

#|macros, connecting wires|#

(define-syntax disj
  (syntax-rules ()
    [(disj) fail]
    [(disj g) g]
    [(disj g₀ g ...) (disj₂ g₀ (disj g ...))]))

(define-syntax conj
  (syntax-rules ()
    [(conj) succeed]
    [(conj g) g]
    [(conj g₀ g ...) (conj₂ g₀ (conj g ...))]))

(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (define (name x ...)
       (λ (s)
         (λ ()
           ((conj g ...) s))))]))

(define-syntax run
  (syntax-rules ()
    [(run n (x₀ x ...) g ...)
     (run n q (fresh (x₀ x ...)
                (== `(,x₀ ,x ...) q)
                g ...))]
    [(run n q g ...)
     (let ([q (var 'q (Scope))])
       (map (reify q) (run-goal n (conj g ...))))]))

(define-syntax run*
  (syntax-rules ()
    [(run* q g ...) (run #f q g ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(fresh () g ...) (conj g ...)]
    [(fresh (x₀ x ...) g ...)
     (call/fresh 'x₀
                 (λ (x₀)
                   (fresh (x ...) g ...)))]))

(define-syntax conde
  (syntax-rules ()
    [(conde (g ...) ...) ((call/new-scope) (disj (conj g ...) ...))]))

(define-syntax condᵉ
  (syntax-rules ()
    [(condᵉ (g ...) ...) ((call/new-scope) (disj (conj g ...) ...))]))

(define-syntax conda
  (syntax-rules ()
    [(conda (g₀ g ...)) (conj g₀ g ...)]
    [(conda (g₀ g ...) ln ...)
     (ifte g₀ (conj g ...) (conda ln ...))]))

(define-syntax condu
  (syntax-rules ()
    [(condu (g₀ g ...) ...)
     (conda ((once g₀) g ...) ...)]))

(define (call/new-scope)
  (λ (g)
    (λS (S @ s scp t neqs abs)
        (g (State s (Scope) t neqs abs)))))

#|other constraints|#

(define ((prt c) S)
  (let ([s (State-s S)])
    (begin (displayln (walk* c s))
           `(,S))))

(define (validate-neqs neqs s)
  (cond
    [(null? neqs) '()]
    [else (go-on ([new-car (unify-all (car neqs) s '())])
            (if (null? new-car)
                #f
                (go-on ([new-cdr (validate-neqs (cdr neqs) s)])
                  (cons new-car new-cdr)))
            (validate-neqs (cdr neqs) s))]))

(define (unify-all ls s new-pairs)
  (cond
    [(null? ls) new-pairs]
    [else (go-on ([`(,s . ,new-pairs)
                   (unify (car (car ls)) (cdr (car ls)) s (Scope) new-pairs)])
            (unify-all (cdr ls) s new-pairs))]))

(define (validate-types ls types)
  (cond
    [(null? ls) types]
    [else (go-on ([types (propogate-type (car ls) types)]
                  [types (validate-types (cdr ls) types)])
            types)]))

(define (propogate-type pr types)
  (let ([u (car pr)]
        [v (cdr pr)])
    (cond
      [(var? v) (let ([u-type (hash-ref types u #f)]
                      [v-type (hash-ref types v #f)])
                  (cond
                    [(and u-type v-type) (and (eqv? u-type v-type) types)]
                    [u-type (hash-set types v u-type)]
                    [v-type (hash-set types u v-type)]
                    [else types]))]
      [else (let ([u-type (hash-ref types u #f)])
              (cond
                [u-type (and (u-type v) types)]
                [else types]))])))

(define (unicons x ls)
  (if (memv x ls) ls (cons x ls)))

(define (not-appears u v neqs abs s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(var? v) (let ([v-abs (hash-ref abs v #f)])
                  (cons (cons `((,v . ,u)) neqs)
                        (hash-set abs v (unicons u (or v-abs '())))))]
      [(pair? v) (go-on ([`(,neqs . ,abs) (not-appears u (car v) neqs abs s)])
                   (not-appears u (cdr v) neqs abs s))]
      [else (and (not (eqv? u v)) (cons neqs abs))])))

(define (validate-abs ls neqs abs s)
  (cond
    [(null? ls) (cons neqs abs)]
    [else (let ([pr (car ls)])
            (let ([u (car pr)]
                  [v (cdr pr)])
              (let ([u-abs (hash-ref abs u #f)])
                (if u-abs
                    (go-on ([`(,neqs . ,abs)
                             (propogate-abs u-abs v neqs abs s)])
                      (validate-abs (cdr ls) neqs abs s))
                    (validate-abs (cdr ls) neqs abs s)))))]))

(define (propogate-abs ls t neqs abs s)
  (cond
    [(null? ls) (cons neqs abs)]
    [else (go-on ([`(,neqs . ,abs) (not-appears (car ls) t neqs abs s)])
            (propogate-abs (cdr ls) t neqs abs s))]))

(define ≡ ==)

(define (=/= u v)
  (λS (S @ s scp t neqs abs)
      (go-on ([`(,s^ . ,new-pairs) (unify u v s (Scope) '())])
        (if (null? new-pairs)
            '()
            `(,(State s scp t (cons new-pairs neqs) abs)))
        `(,S))))

(define ≠ =/=)

(define (booleano u)
  (typeo boolean? u))

(define (numbero u)
  (typeo number? u))
(define numberᵒ numbero)

(define (symbolo u)
  (typeo symbol? u))
(define symbolᵒ symbolo)

(define (typeo pred u)
  (λS (S @ s scp t neqs abs)
      (let ([u (walk u s)])
        (cond
          [(var? u) (let ([u-type (hash-ref t u #f)])
                      (cond
                        [u-type (if (eqv? u-type pred) `(,S) '())]
                        [else `(,(State s scp (hash-set t u pred) neqs abs))]))]
          [(pred u) `(,S)]
          [else '()]))))

(define (absento u v)
  (λS (S @ s scp t neqs abs)
      (go-on ([`(,neqs . ,abs) (not-appears u v neqs abs s)])
        `(,(State s scp t neqs abs))
        '())))
(define absentᵒ absento)


#|syntax sugars|#

(define-syntax go-on
  (syntax-rules ()
    [(_ () then) then]
    [(_ () then alter) then]
    [(_ ([p₀ e₀] [p e] ...) then)
     (cond
       [e₀ => (λ (v) (match v
                       [p₀ (go-on ([p e] ...) then)]))]
       [else #f])]
    [(_ ([p₀ e₀] [p e] ...) then alter)
     (cond
       [e₀ => (λ (v) (match v
                       [p₀ (go-on ([p e] ...) then alter)]))]
       [else alter])]))

(define (State-s S)
  (car S))
(define (State-scp S)
  (car (cdr S)))
(define (State-t S)
  (car (cdr (cdr S))))
(define (State-neqs S)
  (car (cdr (cdr (cdr S)))))
(define (State-abs S)
  (car (cdr (cdr (cdr (cdr S))))))

(define-syntax λS
  (syntax-rules (@)
    [(_ (S @ s scp t neqs abs) b)
     (λ (S)
       (let ([s (State-s S)]
             [scp (State-scp S)]
             [t (State-t S)]
             [neqs (State-neqs S)]
             [abs (State-abs S)])
         b))]))
