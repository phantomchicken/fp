#lang racket
(provide (all-defined-out))
;; MUTACIJE
;; mutirane "škatle" (ne bomo uporabljali)
;; - ustvarjanje: (box v)
;; - dereferenciranje: (unbox box)
;; - prirejanje (set-box! box v)

;; mutirani pari/seznami
;; - testiranje tipa: (mpair? v)
;; - ustvarjanje: (mcons a d)
;; - dereferenciranje: (mcar p) (mcdr p)
;; - prirejanje (set-mcar! p v) (set-mcdr! p v)


;; ZAKASNITEV/SPROŽITEV s predavanj
;; (zakasni thunk)   `thunk` mora biti funkcija brez argumentov, zavijemo v funkcijo
(define (zakasni thunk)
  (mcons #f thunk))
;; (sproži prom)  `prom` mora biti mutiran par,
;;  ki je rezultat klica funcije `zakasni`.

(define (sproži prom)
  (if (mcar prom) ;ali je izracunan
      (mcdr prom) ;ce je, vrni kar vrednost
      (begin (set-mcar! prom #t) ; begin - zaporedje ukazov, nastavi da je izracunano
             (set-mcdr! prom ((mcdr prom))) ;izracunaj drugi del
             (mcdr prom)))) ;vrni izracun


;; Naša implementacija zakasnitev/sprožitev.
(define a (zakasni (λ () (/ 1 2 3 4)))) ;zakasnjeno deljenje
;a -> (mcons #f #<procedure:>)
;(sproži a) -> 1/24
; a -> (mcons #t 1/24)
; a (sproži a) a (sproži a)

;; Racket-ova implementacija zakasnitev/sprožitev.
(define b (delay (λ () 101)))
; b -> #<promise>
; (force b)-> #<procedure>
;((force b)) -> 101
; b (force b) b

(define c (delay 123))
; c -> #<promise>
; (force c) -> 123
; c -> #<promise!123>
; c (force c) c

;; Še nekaj primerov z zakasnitvami.
;; počasno seštevanje
(define (počasno+ a b)
  (begin
    (sleep b)
    (+ a b)))

(define dta (delay (počasno+ 1000 2)))
(define (ta) (počasno+ 1000 2))
;(force dta) -> caka 2s in izracuna
;(ta) -> caka 2s in izracuna
;(force dta) -> vrne rezultat, ker je izracunano


;; THUNKING - primitivno zakasnjevanje
;; (λ () a) (thunk a)


;; TOKOVI - par, prva vr. je vr., druga vr. je rekurzija
;; Kakšna je razlika med `enkef` in `enke`?
(define (enke2) ;funkcija
  (cons 11 enke2))
(define enke; par
  (cons 11 (λ () enke)))

(define enke*
  (cons 11 (thunk enke*))) ; makro za zakasnjevanje

;enke2 -> #<procedure>
;enke -> '(11 . #<procedure>) TOK
;(enke2) -> '(11 . #<procedure>) TOK


;; CIKLIČNI TOK
;; Implementiranj tok `tok123`, ki ga
;;  definira zaporedje 1 2 3 1 2 3 1 ...

;hrani stanje v zaporedju funkcij
; 3 argumenti ki se shiftajo
(define tok123
  (let tok ([a 1] ;zacetne vrednosti
            [b 2]
            [c 3])
    (cons a (thunk (tok b c a)) ))) ;rabimo zakasnitev, drugace nesk. rek.

;(cdr tok123) -> zakasnjen tok
;((cdr tok123)) -> tok


;; Implementiranj funkcijo `ponavljaj`, ki
;;  prejme seznam iz iz njega naredi tok.
;; Primer:
;; (ponavljaj (list 1 2)): 1 2 1 2 1 2...

;lahko kot prej
(define (ponavljaj lst)
  (let loop ([xs lst])
    (match xs
      [(cons h t) (cons h (thunk (loop t)))]
      ['() #:when (not (null? lst)) (loop lst)]))) ;nujno zakasni rekurziven klic, thunk ker je tok


;; PRETVORBA V SEZNAM
;; Implementiranj funkcijo `vzemi`, ki iz prvih
;;  `n` elementov toka zgradi seznam.
;; Primer:
;; (vzemi 4 tok123): '(1 2 3 1)
(define (vzemi n tok)
  (match tok
    [(cons h t) #:when (> n 0) (cons h (vzemi (sub1 n) (t)))] ; velja ko je tok seznam in n>0, tok ni seznam, zavij t v funkcijo
    [_ '()] ;funkcija vraca seznam
    ))


;; ZLIVANJE TOKOV
;; Implementiranj funkcijo `zadrga`,
;;  ki zlije dva tokova v nov tok.
;; Primer:
;; s1: 1 2 3 ...
;; s2: a b c ...
;; (zadrga s1 s2): 1 a 2 b 3 c ...

(define/match (zadrga t1 t2)
  [((cons h1 t1) (cons h2 t2))
   (cons h1 (thunk( cons (h2 (thunk (zadrga (t1) (t2))))))) ]) ;t1 zakasnjen tok, (t1) zakasnjen tok

; (vzemi 10 tok 123)
; (vzemi 20 (zadrga tok123 enke))

; SML OPCIJE S SML SINTAKSO V RACKET-U

(struct some (a) #:transparent)
(struct none () #:transparent)
;(none) -> (none)
;(some 1) -> (some 1)
; (some (some (none))) -> (some (some (none)))


(define-syntax sml
  (syntax-rules (valOf isSome SOME NONE)
    [(sml NONE) (none)]
    [(sml SOME a) (some a)]
    [(sml valOf a) (some-a a)] ; vrni vrednost (atribut a) od a
    [(sml isSome a) (some? a)]))

;makro za realizacijo seznamov v sml-ju, konstruktorji za prazen seznam