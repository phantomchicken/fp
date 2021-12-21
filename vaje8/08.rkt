#lang racket
(provide (all-defined-out))

(struct int (stevilo) #:transparent) ; konstanta, arg je celo stevilo
(struct ?int (e) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ~ (e) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)


(define-syntax geq?
  (syntax-rules ()
  [(geq? e1 e2)
  (?leq e2 e1)]))

(define-syntax ifte
  (syntax-rules (then else)
    [(ifte e1 then e2 else e3)
    (if-then-else e1 e2 e3)]))

(define (fri e)
  (cond [(int? e) e] ; vrnemo izraz v ciljnem jeziku
        [(true? e) e]
        [(false? e) e]
        [(?int? e) (let ([v (fri (?int-e e))]) (if (?int v) (true) (false)))]
        [(~? e)
         (let ([v (fri (~-e e))])
           (cond [(int? v) (int (- (int-stevilo v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (error "negacija nepričakovanega izraza")]))]
        [(add? e)
         (let ([v1 (fri (add-e1 e))]
               [v2 (fri (add-e2 e))])
           (cond [(and (int? v1) (int? v2)) (int (+ (int-stevilo v1) (int-stevilo v2))) ]
                 [(and (true? v1) (true? v2)) (false) ]
                 [(and (true? v1) (false? v2)) (true) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (false) ]
                 [#t (error "seštevanje nepričakovanega izraza")]))]
        [(mul? e)
         (let ([v1 (fri (mul-e1 e))]
               [v2 (fri (mul-e2 e))])
           (cond [(and (int? v1) (int? v2)) (int (* (int-stevilo v1) (int-stevilo v2))) ]
                 [(and (true? v1) (true? v2)) (true) ]
                 [(and (true? v1) (false? v2)) (false) ]
                 [(and (false? v1) (true? v2)) (false) ]
                 [(and (false? v1) (false? v2)) (false) ]
                 [#t (error "množenje nepričakovanega izraza")]))]
        [(?leq? e)
         (let ([v1 (fri (?leq-e1 e))]
               [v2 (fri (?leq-e2 e))])
           (cond [(and (int? v1) (int? v2)) (if (<= (int-stevilo v1) (int-stevilo v2)) (true) (false)) ]
                 [(and (true? v1) (true? v2)) (true) ]
                 [(and (true? v1) (false? v2)) (false) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (true) ]
                 [#t (error "leq nepričakovanega izraza")]))]
        
        [(if-then-else? e) 
         (let ([v-test (fri (if-then-else-condition e))])
           (cond [true? v-test] (fri (if-then-else-e1 e))
                 [false? v-test] (fri (if-then-else-e2 e))
                 [#t (error "pogoj ni logična vrednost")]))]
          
        [#t (error "sintaksa izraza ni pravilna")]))