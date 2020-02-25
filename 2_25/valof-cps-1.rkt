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
       (valof-cps e₁ env
                  (λ (n₁)
                    (valof-cps e₂ env
                               (λ (n₂)
                                 (apply-k cc (+ n₁ n₂))))))]
      [`(let/cc ,kvar ,body)
       #|when valof is cpsed, we don't need Racket's let/cc to implement ours|#
       (valof-cps body (λ (y k) (if (eqv? y kvar) (apply-k k cc) (env y k))) cc)]
      [`(throw ,kexp ,vexp)
       (valof-cps kexp env
                  (λ (k)
                    (valof-cps vexp env k)))]
      [`(λ (,x) ,body)
       (apply-k cc (λ (a k₁) (valof-cps body (λ (y k₂) (if (eqv? y x) (apply-k k₂ a) (env y k₂))) k₁)))]
      [`(,rator ,rand)
       (valof-cps rator env
                  (λ (closure)
                    (valof-cps rand env
                               (λ (a)
                                 (closure a cc)))))])))

(define apply-k
  (λ (cc v)
    (cc v)))



(valof-cps '(+ 1 (let/cc k (((λ (x) (λ (y) (+ y (throw k (+ (throw k y) x))))) 5) 10)))
           (λ (y) (error "unbound " y))
           (λ (v) v))

