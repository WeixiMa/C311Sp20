#lang racket
(require racket/trace)

#|Don't mind this.|#
(define sub2 (compose sub1 sub1))
(define one? (compose zero? sub1))

(define make-k-sub1
  (λ (n k)
    `(sub1 ,n ,k)))

(define make-k-sub2
  (λ (k fib-of-sub1)
    `(sub2 ,k ,fib-of-sub1)))

(define make-k-init
  (λ (jumpout)
    `(init ,jumpout)))

(define make-k-fact
  (λ (n k)
    `(fact ,n ,k)))

(define fib-cps
  (λ (n k)
    (λ ()
      (cond
        [(zero? n) (apply-k k 1)]
        [(one? n) (apply-k k 1)]
        [else (fib-cps (sub1 n)
                       (make-k-sub1 n k))]))))

(define fact-cps
  (λ (n k)
    (λ ()
      (cond
        [(zero? n) (apply-k k 1)]
        [else (fact-cps (sub1 n)
                        (make-k-fact n k))]))))

(define apply-k
  (λ (k v)
    (λ ()
      (match k
        [`(sub1 ,n ,k)
         (fib-cps (sub2 n)
                  (make-k-sub2 k v))]
        [`(sub2 ,k ,fib-of-sub1)
         (apply-k k (+ fib-of-sub1 v))]
        [`(fact ,n ,k) (apply-k k (* n v))]
        [`(init ,jumpout) (jumpout v)]))))

(define trampoline
  (λ (th)
    (trampoline (th))))

(define dt
  (λ (th₁ th₂)
    (dt th₂ (th₁))))

(define lot
  (λ (l)
    (let ([a (car l)]
          [d (cdr l)])
      (lot (append d (list (a)))))))

(let/cc jumpout
  (lot
   (list
    (fact-cps 5 (make-k-init jumpout))
    (fib-cps 5 (make-k-init jumpout))
    (fact-cps 3 (make-k-init jumpout)))))


