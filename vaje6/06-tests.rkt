#lang racket

; Datoteka 06.rkt mora vsebovati vrstico:
; (provide power gcd fib reverse remove map filter zip range is-palindrome)
; oziroma za export celotne datoteke:
; (provide (all-defined-out))

(require rackunit)
(require rackunit/text-ui)
(require "06.rkt")

(define (double-list a)
  (* a 2))

(define (even-list a)
  (= (modulo a 2) 0))

(define (large-list a)
  (> a 50))


(define all-tests
  (test-suite
   "all"
   (test-suite "power"
     (test-case "power 1" (check-equal? (power 2 3) 8))
     (test-case "power 2" (check-equal? (power 3 3) 27))
     (test-case "power 3" (check-equal? (power 5 0) 1))
     (test-case "power 4" (check-equal? (power 5 1) 5))
     (test-case "power 5" (check-equal? (power 1 27) 1))
   )
   (test-suite "gcd"
     (test-case "gcd 1" (check-equal? (gcd 1 1) 1))
     (test-case "gcd 2" (check-equal? (gcd 10 1) 1))
     (test-case "gcd 3" (check-equal? (gcd 16 8) 8))
     (test-case "gcd 4" (check-equal? (gcd 7 3) 1))
     (test-case "gcd 5" (check-equal? (gcd 36 16) 4))
   )
   (test-suite "fib"
     (test-case "fib 1" (check-equal? (fib 1) 1))
     (test-case "fib 2" (check-equal? (fib 5) 5))
     (test-case "fib 3" (check-equal? (fib 8) 21))
     (test-case "fib 4" (check-equal? (fib 50) 12586269025))
   )
   (test-suite "reverse"
     (test-case "reverse 1" (check-equal? (reverse (list 1 2 3)) (list 3 2 1)))
     (test-case "reverse 2" (check-equal? (reverse null) null))
     (test-case "reverse 3" (check-equal? (reverse (list 1 2 3 4 5 6)) (list 6 5 4 3 2 1)))
   )
   (test-suite "remove"
     (test-case "remove 1" (check-equal? (remove 3 (list 1 2 3 4 5 4 3 2 1)) (list 1 2 4 5 4 2 1)))
     (test-case "remove 2" (check-equal? (remove 3 null) null))
     (test-case "remove 3" (check-equal? (remove 10 (list 1 2 3 4 5 6)) (list 1 2 3 4 5 6)))
     (test-case "remove 4" (check-equal? (remove 3 (remove 4 (list 1 2 3 4 5 4 3 2 1))) (list 1 2 5 2 1)))
   )
   (test-suite "map"
     (test-case "map 1" (check-equal? (map double-list (list 1 2 3 4)) (list 2 4 6 8)))
     (test-case "map 2" (check-equal? (map double-list null) null))
     (test-case "map 3" (check-equal? (map double-list (list 8)) (list 16)))
   )
   (test-suite "filter"
     (test-case "filter 1" (check-equal? (filter even-list (list 1 2 3 4 5 6 7 8 9 10)) (list 2 4 6 8 10)))
     (test-case "filter 2" (check-equal? (filter double-list null) null))
     (test-case "filter 3" (check-equal? (filter large-list (list 55 64 87)) (list 55 64 87)))
   )
   (test-suite "zip"
     (test-case "zip 1" (check-equal? (zip (list 1 2 3 4 5) (list 6 7 8 9 10)) (list (cons 1 6) (cons 2 7) (cons 3 8) (cons 4 9) (cons 5 10))))
     (test-case "zip 2" (check-equal? (zip (list 1 2 3 4 5) (list 6 7 8)) (list (cons 1 6) (cons 2 7) (cons 3 8))))
     (test-case "zip 3" (check-equal? (zip null (list 1 2 3)) null))
     (test-case "zip 4" (check-equal? (zip null null) null))
   )
   (test-suite "range"
     (test-case "range 1" (check-equal? (range 1 3 1) (list 1 2 3)))
     (test-case "range 3" (check-equal? (range 1 10 1) (list 1 2 3 4 5 6 7 8 9 10)))
     (test-case "range 3" (check-equal? (range 1 10 1) (list 1 2 3 4 5 6 7 8 9 10)))
     (test-case "range 4" (check-equal? (range 5 10 10) (list 5)))
   )
    (test-suite "is-palindrome"
     (test-case "is-palindrome 1" (check-equal? (is-palindrome null) #t))
     (test-case "is-palindrome 2" (check-equal? (is-palindrome (list 1)) #t))
     (test-case "is-palindrome 3" (check-equal? (is-palindrome (list 1 2)) #f))
     (test-case "is-palindrome 4" (check-equal? (is-palindrome (list 1 2 1)) #t))
     (test-case "is-palindrome 5" (check-equal? (is-palindrome (list 1 1)) #t))
   )
  )
)
(run-tests all-tests)