#|
Change define thunks to define labels.
|#

(define-registers fib-cps-n apply-k-v cc)
(define-program-counter pc)

(define-union continuation
  (init jumpout)
  (sub1 n k)
  (sub2 k v₁))

(define-label fib-cps
  (cond
    [(zero? fib-cps-n)
     (begin [set! cc cc]
            [set! apply-k-v 1]
            (set! pc apply-k))]
    [(zero? (sub1 fib-cps-n))
     (begin [set! cc cc]
            [set! apply-k-v 1]
            (set! pc apply-k))]
    [else (begin [set! cc (continuation_sub1 fib-cps-n cc)]
                 [set! fib-cps-n (sub1 fib-cps-n)]
                 (set! pc fib-cps))]))

(define-label apply-k
  (union-case cc continuation
    [(init jumpout) (dismount-trampoline jumpout)]
    [(sub2 k v₁)
     (begin [set! cc k]
            [set! apply-k-v (+ v₁ apply-k-v)]
            (set! pc apply-k))]
    [(sub1 n k)
     (begin [set! cc (continuation_sub2 k apply-k-v)]
            [set! fib-cps-n (sub1 (sub1 n))]
            (set! pc fib-cps))]))

(define-label main
  (begin 
         [set! fib-cps-n 5]
         (set! pc fib-cps)
         #|mount trampoline after setting everything else|#
         (mount-trampoline continuation_init cc pc)
         (printf "~s\n" apply-k-v)))
