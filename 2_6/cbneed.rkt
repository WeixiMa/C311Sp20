#lang typed/racket/no-check

#|This is a call-by-need interpreter.|#

(define-type Exp
  (U Symbol
     Number
     (List '+ Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))
(define-type Env
  (Listof (Pairof Symbol (Boxof (→ Val)))))
(define-type Closure
  (→ (Boxof (→ Val)) Val))
(define-type Val
  (U Number
     Boolean
     Closure))
(: init-env Env)
(define init-env '())
(: ext-env (→ Symbol (Boxof (→ Val)) Env Env))
(define ext-env
  (λ (x b env)
    `((,x . ,b) . ,env)))
(: apply-env (→ Env Symbol Val))
(define apply-env
  (λ (env y)
    (match env
      ['() (error "unbound " y)]
      [`((,x . ,b) . ,env)
       (if (eqv? y x)
           (let ([th (unbox b)])
             (let ([v (th)])
               (begin (set-box! b (λ () v))
                      v)))
           (apply-env env y))])))

(: valof (→ Exp Env
            Val))
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (apply-env env y)]
      [`,n
       #:when (number? n)
       (begin (displayln 'got-a-number)
              n)]
     [`(+ ,e₁ ,e₂)
       (let ([r₁ (valof e₁ env)]
             [r₂ (valof e₂ env)])
         (cond
           [(and (number? r₁) (number? r₂))
            (+ r₁ r₂)]
           [else (error "don't be silly")]))] 
      [`(λ (,x) ,body)
       (λ ([b : (Boxof (→ Val))]) (valof body (ext-env x b env)))]
      [`(,rator ,x)
       #:when (symbol? x)
       (cond
         [(assv x env)
          =>
          (λ ([pr : (Pairof Symbol (Boxof (→ Val)))])
            (let ([clos (valof rator env)]
                  [b (cdr pr)])
              (cond
                [(or (number? clos) (boolean? clos) (void? clos)) (error "don't be silly")]
                [else (clos b)])))]
         [else (error "unbound " x)])]
      [`(,rator ,rand)
       (let ([clos (valof rator env)]
             [b (box (λ () (valof rand env)))])
         (cond
           [(or (number? clos) (boolean? clos) (void? clos)) (error "don't be silly")]
           [else (clos b)]))])))

(valof '((λ (x) (+ x x)) 5)
       init-env)

(define-syntax kons
  (syntax-rules ()
    [(_ a d) (cons (box (λ () a)) (box (λ () d)))]))
(define-syntax kar
  (syntax-rules ()
    [(_ pr) (let ([b (car pr)])
              (let ([th (unbox b)])
                (let ([a (th)])
                  (begin (set-box! b (λ () a))
                         a))))]))
(define-syntax kdr
  (syntax-rules ()
    [(_ pr) (let ([b (cdr pr)])
              (let ([th (unbox b)])
                (let ([d (th)])
                  (begin (set-box! b (λ () d))
                         d))))]))

(define nats
  (λ (n)
    (kons n (nats (add1 n)))))

(define nat
  (nats 0))

(define take
  (λ ($ n)
    (cond
      [(null? $) '()]
      [(zero? n) '()]
      [(pair? $) (cons (kar $) (take (kdr $) (sub1 n)))]
      [else (take ($) n)])))

#;
(take nat 1000)
