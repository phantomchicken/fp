#lang racket
(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         vars valof fun proc closure call
         greater rev binary filtering folding mapping
         fri)
(require rackunit)
(require rackunit/text-ui)

(struct int (n) #:transparent) ; konstanta, arg je celo n
(struct empty () #:transparent)
(struct ?int (e) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct .. (e1 e2) #:transparent)
(struct exception (exn) #:transparent)
(struct trigger (e) #:transparent)
(struct triggered (e) #:transparent)

(struct ?bool (e) #:transparent)
(struct ?.. (e) #:transparent)
(struct ?seq (e) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?any (e) #:transparent)

(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ?= (e1 e2) #:transparent)
(struct ~ (e) #:transparent)
(struct head (e) #:transparent)
(struct tail (e) #:transparent)

(struct if-then-else (condition e1 e2) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)
(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct call (e args) #:transparent)
(struct closure (env f) #:transparent)

(define (fri e env)
  (cond [(int? e) e]
        [(true? e) e]
        [(false? e) e]
        [(string? e) e]
        [(list? e) e]
        [(empty? e) e]
        [(exception? e) e]
        [(..? e)
         (let ([v1 (fri (..-e1 e) env)]
               [v2 (fri (..-e2 e) env)]) (.. v1 v2)) 
         ]
        [(valof? e) (let ([v (fri (valof-s e) env)])
                      (cond [(triggered? v) v]
                            [(assoc v env) (cdr (assoc v (reverse env)))] ; poisci v okolju in vrni vrednost
                            [#t (triggered (exception "valof: undefined variable"))]))]
        [(?empty? e) (let ([v (fri (?empty-e e) env)])
                       (cond [(triggered? v) v]
                       [(empty? v) (true)]
                       [#t (false)]))]        
        [(?int? e) (let ([v (fri (?int-e e) env)])
                     (cond [(triggered? v) v]
                           [(?int v) (true)]
                           [#t (false)]))]
        [(?bool? e) (let ([v (fri (?bool-e e) env)])
                      (cond [(triggered? v) v] 
                            [(or (true? v) (false? v)) (true)]
                            [#t (false)]))]
        [(?exception? e) (let ([v (fri (?exception-e e) env)])
                           (cond [(triggered? v) v]
                                 [(exception? v) (true)]
                                 [#t (false)]))] 
        [(?seq? e) (let ([v  (fri (?seq-e e) env)])
                     (cond [(triggered? v) v]
                           [(empty? v) (true)] ;true le ko se konca z empty
                           [(..? v) (fri (?seq (..-e2 v)) env) ] ;nadaljuj, preveri ce se konca z empty
                           [#t (false)]))]
                 
        [(?..? e) (let ([v (fri (?..-e e) env)])
                    (cond [(triggered? e) e]
                          [(..? v) (true)]
                          [#t (false)]))]
        [(?=? e)
         (let ([v1 (fri (?=-e1 e) env)]
               [v2 (fri (?=-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(equal? (v1 v2)) (true)]
                 [#t (false)]))]
        [(trigger? e) (let ([exn (fri (trigger-e e) env)])
                        (cond [(triggered? exn) exn] ;ce je sprozena, jo vrnemo
                              [(exception? exn) (triggered exn)] ;ce ni sprozena, jo sprozimo
                              [#t (triggered (exception "trigger: wrong argument type"))]))]
                 
        [(~? e)
         (let ([v (fri (~-e e) env)])
           (cond [(triggered? v) v]
                 [(int? v) (int (- (int-n v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (triggered (exception "~: wrong argument type"))]))]
        
        [(head? e) (let ([v (fri (head-e e) env)])
                     (cond [(triggered? v) v]
                       [(empty? v) (triggered (exception "head: empty sequence"))]
                       [(equal? (fri (?.. v) env) (true)) (..-e1 v)] ;ce je v zaporedje vrni prvi del zaporedja                          
                       [#t (triggered (exception "head: wrong argument type"))]) ) ]
        [(tail? e) (let ([v (fri (tail-e e) env)])
                     (cond [(triggered? v) v]
                       [(empty? v) (triggered (exception "tail: empty sequence"))]
                       [(equal? (fri (?.. v) env) (true)) (..-e2 v)] ;ce je v zaporedje vrni drugi del zaporedja                          
                       [#t (triggered (exception "tail: wrong argument type"))]) ) ]
        [(add? e)
         (let ([v1 (fri (add-e1 e) env)]
               [v2 (fri (add-e2 e) env)])
           (cond [(triggered? v1) v1] ; ce je sumand sprozena izjema, ga vrnemo
                 [(triggered? v2) v2]
                 [(empty? v1) v2] ; ce je sumand prazen vrnemo drugega
                 [(empty? v2) v1]
                 [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2))) ] ; ce sta sumanda stevili/booli, jih sestejemo
                 [(and (true? v1) (true? v2)) (false) ]
                 [(and (true? v1) (false? v2)) (true) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (false) ]                 
                 [(and (equal? (fri (?seq v1) env) (true)) (equal? (fri (?seq v2) env) (true))) (concat v1 v2 env)] ; ce sta sumanda zaporedji
                 [#t (triggered (exception "add: wrong argument type"))]))]
        [(mul? e)
         (let ([v1 (fri (mul-e1 e) env)]
               [v2 (fri (mul-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(and (int? v1) (int? v2)) (int (* (int-n v1) (int-n v2))) ]
                 [(and (true? v1) (true? v2)) (true) ]
                 [(and (true? v1) (false? v2)) (false) ]
                 [(and (false? v1) (true? v2)) (false) ]
                 [(and (false? v1) (false? v2)) (false) ]
                 [#t (triggered (exception "mul: wrong argument type"))]))]
        [(?leq? e)
         (let ([v1 (fri (?leq-e1 e) env)]
               [v2 (fri (?leq-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(and (int? v1) (int? v2)) (if (<= (int-n v1) (int-n v2)) (true) (false)) ]
                 [(and (true? v1) (true? v2)) (true) ]
                 [(and (true? v1) (false? v2)) (false) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (true) ]
                 [(and (equal? (fri (?seq v1) env) (true)) (equal? (fri (?seq v2) env) (true))) (if (<= (len v1) (len v2)) (true) (false)) ] ; ce sta argumenta zaporedji, primerjaj dolzino
                 [#t (triggered (exception "?leq: wrong argument type"))]))]
        [(?all? e)
         (let ([v (fri (?all-e e) env)])
           (cond [(triggered? v) v]
                 [(empty? v) (true)] ; ce je zaporedje prazno - true, sicer, preveri ce je glava false in rekurzivno nadaljuj. Preveri tudi ce so samo booli
                 [(..? v) (if (onlybools (..-e2 v)) (if (false? (fri (..-e1 v) env)) (false)  (fri (?all (..-e2 v)) env)  ) (triggered (exception "?all: wrong argument type")))  ]
                 [#t (triggered (exception "?all: wrong argument type"))]))]
        [(?any? e)
         (let ([v (fri (?any-e e) env)])
           (cond [(triggered? v) v] ; ce je zaporedje prazno - false, sicer, preveri ce je glava true in rekurzivno nadaljuj. Preveri tudi ce so samo booli
                 [(empty? v) (false)]
                 [(..? v) (if (onlybools (..-e2 v)) (if (true? (fri (..-e1 v) env)) (true)  (fri (?all (..-e2 v)) env)  ) (triggered (exception "?any: wrong argument type")))  ]
                 [#t (triggered (exception "?any: wrong argument type"))]))]
      
        [(if-then-else? e)
         (let ([v (fri (if-then-else-condition e) env)])
           (cond [(false? v) (fri (if-then-else-e2 e) env)] ; else edino v primeru ko je pogoj false
                 [#t (fri (if-then-else-e1 e) env)]))]
        [(handle? e) (let ([v1 (fri (handle-e1 e) env)]
                           [v2 (fri (handle-e2 e) env)]
                           [v3 (handle-e3 e)])
                       (cond [(triggered? v1) v1] ; v1 sprozena izjema, vrni v1
                             [(not (exception? v1)) (triggered (exception "handle: wrong argument type"))] ; ce v1 ni izjema, sprozi izjemo wrong arg
                             [(and (triggered? v2) (equal? (triggered-e v2) v1) ) (fri v3 env)] ; ce je v2 sprozena izjema enaka v1, vrni v3
                             [#t v2])) ] ; sicer izvedi v2
        [(vars? e) (let ([s (fri (vars-s e) env)] ; ime spremenljivk
                         [e1 (fri (vars-e1 e) env)] ; vrednost spremenljivk
                         [e2 (vars-e2 e)]) ; evalvacija spremenljivk
                     (cond [(triggered? s) s] ; ce sprozena izjema v s,e1 vrni izjemo
                           [(triggered? e1) e1]                          
                           [(and (list? s) (list? e1)) (fri e2 (append env (concatenv s e1 env null)))] ; ce dodajamo vec spremenljivk dodaj seznam parov v okolje
                           [#t (fri e2 (append-test env (list (cons s e1))))]))] ;sicer dodaj par (spremenljivka-vrednost) v okolje               
        [(fun? e) (closure env e)]
        [(proc? e) e]
        [(closure? e) e]        
        [(call? e) (let ([ok (fri (call-e e) env)] ; okolje in argumenti
                         [args (call-args e)])
                     (cond [(triggered? ok) ok] ; ce je okolje sprozena izjema, vrni okolje
                           [(proc? ok) (fri (proc-body ok) (append-test env (list (cons (proc-name ok) ok))))] ;ce klicemo proc, izvedi telo na okolju, dodaj ime procedure in evalvirano okolje v okolje env
                           [(closure? ok)
                            (let ([c-f (closure-f ok)]) ;izvedi vars (argumenti ovojnice, argumenti, telo funkcije), dodaj ime funkcije in ovojnico v okolje env
                              (fri (vars (fun-farg c-f) args (fun-body c-f)) (append-test env (list (cons (fun-name c-f) ok)))))]
                           [#t (triggered (exception "call: wrong argument type"))]))]                   

        [#t (error "sintaksa izraza ni pravilna")]))

(define (concatenv s e1 env1 env2)
    (cond [(or (null? s) (null? e1)) env2] ; konec seznama sprem. vrni novo okolje env2
          [(and (list? s) (list? e1))
           (let ([car-s (car s)] ;ime sprem.
                 [car-e1 (car e1)] ;vrednost sprem.
                 [cdr-s (cdr s)] ; rep seznama imen sprem.
                 [cdr-e1 (cdr e1)]) ; rep vrednosti sprem.
             (if (triggered? (fri car-e1 env1)) ; ce dodajamo sprozeno izjemo jo vrnemo
                 (fri car-e1 env1)
                 (concatenv cdr-s cdr-e1 env1 (append env2 (list (cons (fri car-s env1) (fri car-e1 env1)))) ; rekurzivno nadaljuj, zdruzi rep imen, rep vrednosti z rezultatom zdruzevanja env2 in trenutnega para ime-vr.
)))]))

(define (concat xs ys env) ; zdruzi fri zaporedji
           (cond [(and (empty? xs) (empty? ys)) (empty)] ;ce sta seznama prazna je rez prazen
                 [(and (empty? xs) (..? ys)) ys] ; ce je le eden prazen je rez drugi
                 [(and (..? xs) (empty? ys)) xs] 
                 [(and (..? xs) (..? ys)) (fri (.. (fri (head xs) env) (concat (fri (tail xs) env) ys env)) env)] ; glava prvi + rekurzija (rep prvi, drugi), na koncu je prvi prazen
                 
           ))

(define (append-test lhs rhs) ; zdruzi racket seznama
  (if (null? lhs)
      rhs
      (cons (first lhs) (append-test (rest lhs) rhs))))

(define (len xs) ; vrne dolzino zaporedja
     (cond [(empty? xs) 0]
           [(..? xs) (+ 1 (len (..-e2 xs)))]
           [#t 0]))

(define (onlybools xs) ; vrne (true) ce je zaporedje zaporedje boolov
  (cond [(empty? xs) #t]
        [(equal? (head xs) (true)) (onlybools (tail xs)) ]
        [(equal? (head xs) (false)) (onlybools (tail xs)) ]
        [#t #f]))

(define (greater e1 e2)
  (if-then-else (?= e1 e2) (false) (?leq e2 e1)))

(define (filtering f seq)  ; poklici filter s funkcijo f na seznamu seq
  (call (fun "filtering" (list "f" "seq")
             (if-then-else (?empty (valof "seq")) ; ce seznam prazen vrni empty
                           (empty)
                           (if-then-else (call (valof "f") (list (head (valof "seq")))) ; ce predikat vrne true za glavo seq, zdruzi glavo in rezultat rekurzivnega klica                          
                                         (.. (list (head (valof "seq"))) (call (valof "filtering") (list (valof "f") (tail (valof "seq")))))
                                         (.. (empty) (call (valof "filtering") (list (valof "f") (tail (valof "seq")))))))) (list f seq))) ;sicer vrni rezultat rekurzivnega klica

(define (folding f init seq)
  (call (fun "folding" (list "f" "init" "seq") ;poklici fold s funkcijo f in akumulatorjem init na seznamu seq
             (if-then-else (?empty (valof "seq")) ; ce seznam prazen vrni init
                           (valof "init")                           
                           (call (valof "folding") ; sicer rekurzivno nadaljuj, (ista f, poklici f na glavi seq in zdruzi z initom, rep od seq)
                                 (list (valof "f")
                                 (call (valof "f") (list (head (valof "seq")) (valof "init")))
                                 (tail (valof "seq"))) )))
        (list f init seq)))

(define (mapping f seq)
  (call (fun "mapping" (list "f" "seq") ;poklici map s funkcijo f na seznamu seq
             (if-then-else (?empty (valof "seq")) ; ce seznam prazen vrni empty
                           (empty)                           
                           (.. (call (valof "f") (list (head (valof "seq")))) ;sicer zdruzi v seznam evalviran klic na glavi seq ter rekurzijo (map, ista f, rep seq)
                               (call (valof "mapping") (list (valof "f") (tail (valof "seq")))) ))) (list f seq)))


(define (binary) 1)

#|(define (rev lst)
  (foldl cons null lst))|#

(define (rev lst) (folding (fun "cons" (list "e1" "e2") (.. (valof "e1") (valof "e2"))) (empty) lst))