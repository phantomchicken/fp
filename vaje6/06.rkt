#lang racket
(provide power gcd fib reverse remove map filter zip range is-palindrome)
;1 (power 2 3)
(define (power a n)
  (if (zero? n)
      1
      (* a (power a (sub1 n)) )
  ))

;2 (gcd 7 3)
(define (gcd a b)
  (if (zero? b)     
      a
      (gcd b (modulo a b))
   ))

;3 (fib 3)
(define (fib n)
  (cond [(= n 1) 1 ]
        [(= n 2) 1]
        [else (+ (fib (sub1 n)) (fib (- n 2))) ]))

;4 (reverse (list 1 2 3))
(define (reverse lst)
  (foldl cons null lst))

;5 (remove 3 (list 1 2 3 4 5 4 3 2 1))
(define (remove el xs)
  (cond [(null? xs) xs]
        [(= (first xs) el) (remove el (rest xs))]
        [#t (cons (first xs) (remove el (rest xs)))])
  )

#|(match xs
    ['() '()]
    [(cons el t) (remove el t)]
    [(cons h t) (cons h (remove el t))]
    )|#

;6 (map (lambda (a) (* a 2)) (list 1 2 3)) 
(define(map f xs)
  (if (null? xs)
      xs
      (cons (f(first xs))  (map f (rest xs)))
  ))

;7 (filter (lambda (a) (= (modulo a 2) 0)) (list 1 2 3))
(define (filter f xs)
  (if (null? xs)
      xs
      ( if (f (first xs))
           (cons (first xs)  (filter f (rest xs)))
           (filter f (rest xs))
           )))

;8 (zip (list 1 2 3) (list 4 5 6))
(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      null
      (cons (cons (first xs) (first ys)) (zip (rest xs) (rest ys)) )
      ))

;9 (range 1 3 1)
(define (range z k korak)
  (if (< k z)
       null
      (cons z (range (+ z korak) k korak)
      )
  ))

;10 (is-palindrome (list 2 3 5 1 6 1 5 3 2))
(define (rev lst)
  (foldl cons null lst))

(define (is-palindrome xs)
  (equal? xs (rev xs))
  )

