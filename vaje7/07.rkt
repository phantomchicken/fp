#lang racket

;; - veži število 5 v spremenljivko `pet`
(define pet 5)

;; Racket je dinamično tipiziran.
;; - preveri ali je `pet` število/celo število na dva načina.
;podcrtaj je vrednost, zelene vrednosti so konstante, racket je language oriented jezik
(number? pet)
(integer? pet)


;; Uporabne bljižnice: c:a, c:/, F1, c:i
;; - veži število 1/5 v spremenljivko `petina` na dva načina.
; (+ 1 3 4)
; (< 1 3 4 5 6 7)
; (sqrt -4) vrne kar -2i
(define petina 1/5)
(define petina2 (/ 5))

;; Poglej dokumentacijo za funkcijo `/`
;; Izračunaj koliko let imaš, če si star 200000 ur.
(/ 200000 24 365)

;; - shrani vrednost meta (6-strane) kocke v spremenljivko `kocka`
;;   Uporabim `random`
(define kocka (random 1 6))
;kocka, random, ranodm ( min max)

;; - veži anonimno funkcijo v spremenljivko `anonimna`,
;;   ki vrne vrednost spremenljivke `kocka`
(define anonimna (lambda () kocka))
;anonimna->proc, (anonimna)->4, ((anonimna)) -> klic anonimne (4)
; (define (poimenovana) kocka)
(define f0 (lambda (a) (- a))) ; natanko 1 arg
(define f1 (lambda (a b) (+ a b))) ; natanko 2 arg
(define f2 (lambda a a)) ; natanko 2 arg
;(define f3 (lambda _ a)) ; poljubno arg
;(define f4 (lambda () a)) ; natanko 0 arg
(define f3 (lambda (a . b) a)) ; vrne prvi arg
(define f4 (lambda (a c . b) c)) ; vrne drugi arg

;; - definiranj funkcijo vrži-kocko
(define (vrzi-kocko) (random 1 6))
; vrzi-kocko -> proc, (vrzi-kocko) - rand st

;; - definiraj funkcijo `add`, ki zna sešteti dve števili
(define (add x y) (+ x y))

;; - Kakšna je razlika med `addc`, `addc2`, `addc3`?
(define addc
  (λ (x) (λ (y) (+ x y)))) ;sprejme en arg - vrne f
(define (addc2) ; sprejme 0 arg
  (λ (x) (λ (y) (+ x y)))) ; sprejme en arg - vrne f
(define ((addc3 x) y) (+ x y)) ; sprejme 1 arg
(define ((addc31 x y) z) (+ x y)) ; sprejme 2 arg vrne f

;; - definiranj funkcijo `n-krat-vrži-kocko` (vrne vsoto n-tih metov)
; if za eno moznost, cond za vec, = n 0 ali zero?, false samo ko #f, sicer #t
; - n 1 ali sub1 n
(define (vrzi-nkrat n)
  (if (zero? n)
      0
      (+ (vrzi-kocko) (vrzi-nkrat (sub1 n) ))))

;; - definiranj funkcijo `seznam-n-metov` (vrne seznam n-tih metov)
;; Prazen seznam: '(), null, (list)
;; Seznam z enim elementom `e`: (cons e null), (list e).
;; Seznam je zaporedje parov.
(define (seznam-n-metov n)
  (cond [(zero? n) null]
        [else (cons (vrzi-kocko) (seznam-n-metov (sub1 n) ))]))

;; alternativa
(define (seznam-n-metov2 n) (build-list n (λ _ (vrzi-kocko))))

;; alternativa seznam raspakiramo in sestejemo
(define (n-krat-vrži-kocko3 n) (apply + (seznam-n-metov2 n)))

;; - definiranj funkcijo `rev` z uporabo akumulatorjev
;car - first, cdr - rest, list raje kot '()

;acc
(define (rev xs)
  (define (rev xs acc)
    (cond [(null? xs) acc]
          [else (rev (rest xs) (cons (first xs) acc))]))
  (rev xs null))

; letrec
(define (rev2 xs)
  (letrec ([rev (lambda (xs acc)
                  (cond [(null? xs) acc]
                        [else (rev (rest xs) (cons (first xs) acc))]))])
    (rev xs null)))
;(rev2 null) -> ok
; (rev2 (list 1 2 3)) -> err vzame prejsnji rev ZATO LETREC namesto LET!

; let
(define (rev3 lst)
  (let rev([xs lst] ; xs je lst, acc je null
           [acc null])
    (cond [(null? xs) acc]
          [else (rev (rest xs) (cons (first xs) acc))])))

; foldl
(define (rev4 lst)
  (foldl cons null lst))
;(rev4 (list 2 3 4))

; match
(define (rev5 xs)
  (define (rev xs acc)
    (match xs
      ['() acc] ;'() raje kot null v matching!!!
      [(cons h t) (rev t (cons h acc))]))
  (rev xs null))

;; - definiranj funkcijo `preštej`,
;;   ki prešteje število argumentov
(define (prestej . vsi-argumenti)
  (length vsi-argumenti))

; <rešitve>
; <rešitve>


;; - definiranj funkcijo `skalarni-produkt`:
;; (skalarni-produkt (list 1 -1 1) (list 2 -4 -4)) -> 2
(define (skalarni-produkt xs ys)
(foldl (lambda (x y acc) (+ (* x y) acc)) 0 xs ys))

;; - definiranj funkcijo `binary`:
;; (binary 0) -> 0
;; (binary 1 0 1 0) -> 10
;; (binary 1 0 1 0 1 0) -> 42
; <rešitve>

;; definiranj funkcijo `tetration a n`
;; https://en.wikipedia.org/wiki/Tetration
(define (tetration a n) 1
  (if (zero? n) 1 (expt a (tetration a (sub1 n)))))
; nedokočana implementacija
; <rešitve>


;; risanje grafov
(require plot)
(parameterize ([plot-width    800]
               [plot-height   800]
               [plot-x-label  "im(x)"]
               [plot-y-label  "re(x)"])
  (define x (build-list 40 (λ (i) (tetration 0+i i))))
  (plot (points (map (λ (a) (vector (real-part a) (imag-part a))) x))
        #:x-min -0.1 #:x-max 1.1
        #:y-min -0.1 #:y-max 1.1))