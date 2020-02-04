#lang typed/racket
(require typed/rackunit)

(define-type Store (Listof (Pairof Real Real)))
#;
(: store (Listof (Pairof Real Real)))
#;
(define store
  '())

#;
(: fib (→ Real Real))
#;
(define fib
  (λ (n)
    (cond
      [(assv n store)
       =>
       (λ ([pr : (Pairof Real Real)])
         (cdr pr))]
      [(zero? n) 1]
      [(zero? (sub1 n)) 1]
      [else (let ([r₁ (fib (sub1 n))]
                  [r₂ (fib (sub1 (sub1 n)))])
              (let ([r (+ r₁ r₂)])
                (begin (set! store `((,n . ,r) . ,store))
                       r)))])))

#;
(let* ([x 5]
       [y x])
  (+ x y))

(define-syntax define/trace
  (syntax-rules ()
    [(_ name (λ (arg ...) body))
     (define name
       (λ (arg ...)
         (begin
           (displayln 'name)
           (displayln arg) ...
           (displayln "")
           body)))]))

(: fib (→ Real Store
          (List Real Store)))
(define/trace fib
  (λ (n store)
    (cond
      [(assv n store)
       =>
       (λ ([pr : (Pairof Real Real)])
         (list (cdr pr) store))]
      [(zero? n) (list 1 store)]
      [(zero? (sub1 n)) (list 1 store)]
      [else (match-let* ([`(,r₁ ,store) (fib (sub1 n) store)]
                         [`(,r₂ ,store) (fib (sub1 (sub1 n)) store)])
              (let ([r (+ r₁ r₂)])
                (list r `((,n . ,r) . ,store))))]
      #;
      [else (let ([r₁ (fib (sub1 n))]
                  [r₂ (fib (sub1 (sub1 n)))])
              (let ([r (+ r₁ r₂)])
                (begin (set! store `((,n . ,r) . ,store))
                       r)))])))


(: best-price (→ (Listof Real) Real
                 (List Real Real)))
(define best-price
  (λ (l highest)
    (cond
      [(null? l) (list 0 highest)]
      [else (match (best-price (cdr l) highest)
              [(list max-profit highest)
               (let ([max-profit (max max-profit (- highest (car l)))]
                     [highest (max (car l) highest)])
                 (list max-profit highest))])])))

(define-syntax test-runner
  (syntax-rules (>)
    [(_) 'done]
    [(_ > test result more ...)
     (begin (check-equal? test 'result)
            (test-runner more ...))]))

(: remv-1st (→ Symbol (Listof Symbol)
               (Listof Symbol)))
(define remv-1st
  (λ (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? (car ls) x) (cdr ls)]
      [else (cons (car ls) (remv-1st x (cdr ls)))])))

(test-runner
> (remv-1st 'x '(x y z x))
(y z x)
> (remv-1st 'y '(y z y x))
(x z y x)
> (remv-1st 'z '(a b c))
(a b c))
