#lang racket
(struct avto (tip barva) #:transparent)

; (avto "audi" "crn") - konstruktor
;(define a (acto "audi" "crn"))
; (avto? a) - funkcija preverjanja tipa
;(avto-tip a) - getter
;(avto-barva) - getter

;#transparent - vpogled v implementacijo

(struct konst (int) #:transparent) ; konstanta, arg je celo stevilo
(struct bool (b) #:transparent) ; logicna vr., b je #t ali #f
(struct negiraj (e) #:transparent); e je izraz, ki ga negiramo
(struct sestej (e1 e2) #:transparent); e1 in e2 sta izraza, ki ju sestevamo
(struct ce-potem-sicer (pogoj res nires) #:transparent)

;sencenje
(struct beri () #:transparent)
(struct shrani (vrednost izraz) #:transparent) ;+ a 15 izraz, let [a 3] izraz

;[(shrani? e) (jais (shrani-izraz e) (jais (shrani-vrednost e ) env))] ; nujno jais shrani-vrednost, drugace shrani sestej 3 4 v okolje namesto 7
;(jais2 (shrani (konst 143) (beri))) -> (konst 143)
;(jais2 (shrani (konst 14) (sestej (konst 1) (beri)))) -> konst 15, okolje je 14 sestejem z 1

;(jais2 (shrani (konst 14) (shrani (konst 2) (shrani (bool #f) (negiraj (beri))))))
; env-> konst 14 -> konst 2 -> bool #f
;(jais2 (shrani (konst 14) (sestej (beri) (shrani (konst 2) (sestej (beri) (konst1)))))) -> 14+ 2+1=17


;(konst 5)
; (bool #t);
;(negiraj (sestej (konst 3) (konst 5)))

;to so goli izrazi, manjka nam interpreter
; intepreter konst 3 -> 3



(define (jais2 e)
  (letrec ([jais (lambda (e env)
                 (cond [(konst? e) e] ; vrnemo izraz v ciljnem jeziku
                       [(bool? e) e]
                       [(negiraj? e)
                        (let ([v (jais (negiraj-e e) env)])
                          (cond [(konst? v) (konst (- (konst-int v)))]
                                [(bool? v) (bool (not (bool-b v)))]
                                [#t (error "negacija nepričakovanega izraza")]))]
                       [(sestej? e)
                        (let ([v1 (jais (sestej-e1 e) env)]
                              [v2 (jais (sestej-e2 e) env)])
                          (if (and (konst? v1) (konst? v2))
                              (konst (+ (konst-int v1) (konst-int v2)))
                              (error "seštevanec ni številka")))]
                       [#t (error "sintaksa izraza ni pravilna")]))
    (jais e null)])))

;(jais2

  
  (define (jais e)
    (cond [(konst? e) e] ; vrnemo izraz v ciljnem jeziku
          [(bool? e) e]
          [(negiraj? e)
           (let ([v (jais (negiraj-e e))])
             (cond [(konst? v) (konst (- (konst-int v)))]
                   [(bool? v) (bool (not (bool-b v)))]
                   [#t (error "negacija nepričakovanega izraza")]))]
          [(sestej? e)
           (let ([v1 (jais (sestej-e1 e))]
                 [v2 (jais (sestej-e2 e))])
             (if (and (konst? v1) (konst? v2))
                 (konst (+ (konst-int v1) (konst-int v2)))
                 (error "seštevanec ni številka")))]
          [#t (error "sintaksa izraza ni pravilna")]))


  ;(jais (sestej (konst 3) (konst 5)))
  ; (konst-int (konst 3))

  ;sestej vrne racketov izraz - konst, ki je rezultat sestavanja konst-int
  ;kaj ce ni int? preveri tip!

  ;omogoci sestavljene izraze v (jais (ce-potem-sicer-pogoj e) namesto #t #f


;------------------------- SPREMENLJIVKE
; seznam parov: ime - vrednost
; assoc vrne vrednost
; env '(("a" (konst 5) ("b" (bool #f))
; assoc env "b" -> dostopa do b

; (cons ("x" (konst 8) env) -> deklaracija spremenljivke
; ali ze obstaja? assoc vrne true ali false
; 1) popravi beri - assoc memoizacija
; 2) shrani - env s consom, dodaj novi par

;------------------------- FUNKCIJE
;klici - ovojnica (okolje funkcija (ime telo))
(struct funkcija (ime telo) #:transparent)
(struct ovojnica (okolje fun) #:transparent)
(struct klici (ovojnica) #:transparent)

[(funkcija? e) (ovojnica env e)]
;(jais2 (funkcija "ime" (konst 3))) -> ovojnica '() (funkcija "ime" (konst 3)))
;(jais2 (shrani (konst 14) (shrani (konst 42) (funkcija "ime" (konst 3))))) -> (ovojnica (konst 42) (funkcija "ime" (konst 3)))
;(jais2 (klici))

[(klici? e) (let ([o (jais (klici-ovojnica e) env)])
              (if (ovojnica? o)
                  (jais (funkcija-telo (ovojnica-fun o))
                        (ovojnica-okolje o))
                  (error "Klic funkcije je napacen!")))]
;(jais2 (klici (funkcija "ime" (konst 3)))) -> (konst 3), prazno okolje
;(jais2 (klici (funkcija "ime" (beri)))) -> '()
;(jais2 (shrani (konst 42 (klici (funkcija "ime" (beri)))))) -> (konst 42)

;-------------------------OPTIMIZACIJA OVOJNICE
(lambda (a) (+ a b c)) ;b in c nujni
(lambda (a) (let ([b 5]) (+ a b c) )) ;c nujen
(lambda (a) (+ b (let ([b c]) (* b 5)))) ;b nujen zunanji, c nujen

;-------------------------MAKRI
(define trikratnik
  (sestej x (sestej x x)))

(define (in e1 e2)
  (ce-potem-sicer e1 e2 (bool #f)))

(define (ali e1 e2)
  (ce-potem-sicer e1 (bool #t) e2))

(define (xor e1 e2)
  (ce-potem-sicer e1 (ce-potem-sicer e2 (bool #f) (bool #t)) e2))

(define (vsota-sez sez)
  )

;makro razsiritev
;(trikratnik (konst 5)) -> (sestej (konst 5) (sestej (konst 5) (konst 5)))
;(jais2 (trikratnik (konst 5)) -> (konst 15) 

;higienicen? imena spremenljivk?

;------------------------- SEMINARSKA
; fri je jais, exp je e, env environment
; operacije
; spremenljivke v seznamih
