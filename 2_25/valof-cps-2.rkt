#lang racket

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
      [`(letcc ,kvar ,body)
       #|cc means current-continuation|#
       (let/cc cc (valof body (λ (y) (if (eqv? y kvar) cc (env y)))))]
      [`(throw ,kexp ,vexp)
       ((valof kexp env) (valof vexp env))]
      [`(λ (,x) ,b)
       (λ (a) (valof b (λ (y) (if (eqv? y x) a (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

#;
(valof '(+ 1 (letcc k (((λ (x) (λ (y) (+ y (throw k (+ (throw k y) x))))) 5) 10)))
       (λ (y) (error "unbound " y)))

(define valof-cps
  (λ (exp env cc)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y cc)]
      [`,n
       #:when (number? n)
       (apply-k cc n)]
      [`(+ ,e₁ ,e₂)
       (valof-cps e₁ env (make-k-+-e₁ e₂ env cc))]
      [`(let/cc ,kvar ,body)
       #|when valof is cpsed, we don't need Racket's let/cc to implement ours|#
       (valof-cps body (λ (y k) (if (eqv? y kvar) (apply-k k cc) (env y k))) cc)]
      [`(throw ,kexp ,vexp)
       (valof-cps kexp env (make-k-throw vexp env))]
      [`(λ (,x) ,body)
       (apply-k cc (λ (a k₁) (valof-cps body (λ (y k₂) (if (eqv? y x) (apply-k k₂ a) (env y k₂))) k₁)))]
      [`(,rator ,rand)
       (valof-cps rator env (make-k-rator rand env cc))])))

(define make-k-+-e₁
  (λ (e₂ env cc)
    (λ (n₁)
      (valof-cps e₂ env (make-k-+-e₂ cc n₁)))))

(define make-k-+-e₂
  (λ (cc n₁)
    (λ (n₂)
      (apply-k cc (+ n₁ n₂)))))

(define make-k-throw
  (λ (vexp env)
    (λ (k)
      (valof-cps vexp env k))))

(define make-k-rator
  (λ (rand env cc)
    (λ (closure)
      (valof-cps rand env (make-k-rand closure cc)))))

(define make-k-rand
  (λ (closure cc)
    (λ (a)
      (closure a cc))))

(define make-init-k
  (λ ()
    (λ (v) v)))

(define apply-k
  (λ (cc v)
    (cc v)))


(valof-cps '(+ 1 (let/cc k (((λ (x) (λ (y) (+ y (throw k (+ (throw k y) x))))) 5) 10)))
           (λ (y) (error "unbound " y))
           (make-init-k))

