#lang racket
(define (power a n)
  (if (zero? n)
      1
      (* a (power a (sub1 n)) )
  ))


(define (gcd a b)
  (if (zero? b)     
      a
      (gcd b (modulo a b))
   ))

(define (fib n)
  (cond [(= n 1) 1 ]
        [(= n 2) 1]
        [else (+ (fib (sub1 n)) (fib (- n 2))) ]))

(define (reverse lst)
  (foldl cons null lst))


(define (remove el xs)
  (match xs
    ['() '()]
    [(cons el t) (remove el t)]
    [(cons h t) (cons (h (remove el t)))]
    )
  )
#|(define (rev xs acc)
    (match xs
      ['() acc] ;'() raje kot null v matching!!!
      [(cons h t) (rev t (cons h acc))]))
  (rev xs null)) |#

#|(define (gcd a b)
  (if (zero? b)
      a
      (gcd (b (modulo a b)))))|#

#| fun gcd (a : int, b : int) : int =
        if b<>0 then gcd(b, a mod b)
        else a;|#