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


(define-syntax geq?
  (syntax-rules ()
  [(geq? e1 e2)
  (?leq e2 e1)]))

(define-syntax ifte
  (syntax-rules (then else)
    [(ifte e1 then e2 else e3)
    (if-then-else e1 e2 e3)]))

(define (fri e env)
  (cond [(int? e) e] ; vrnemo izraz v ciljnem jeziku
        [(true? e) e]
        [(false? e) e]
        [(string? e) e]
        [(list? e) e]
        [(empty? e) e]
        [(exception? e) e]
        [(valof? e) (let ([v (fri (valof-s e) env)])
                      (cond [(triggered? v) v]
                            [(assoc v env) (cdr (assoc v (reverse env)))]
                            [#t (triggered (exception "valof: undefined variable"))]))]
        [(?empty? e) (let ([v (fri (?empty-e e) env)]) (if (empty? v) (true) (false)))]        
        [(?int? e) (let ([v (fri (?int-e e) env)]) (if (?int v) (true) (false)))]
        [(?bool? e) (if (or (true? (fri (?bool-e e) env) (false? (fri (?bool-e e) env)))) (true) (false))]
        [(?exception? e) (let ([v (fri (?exception-e e) env)]) (if (exception? v) (true) (false)))] 
        [(?seq? e) (if (or (..? (fri (?seq-e e) env)) (empty? (fri (?seq-e e) env))) (true) (false))]
        [(?..? e) (let ([v (fri (?..-e e) env)]) (if (empty? v) (false) (true)))]
        [(trigger? e) (let ([exn (fri (trigger-e e) env)])
                        (cond [(triggered? exn) exn]
                              [(exception? exn) (triggered exn)]
                              [#t (triggered (exception "trigger: wrong argument type"))]))]
        [(~? e)
         (let ([v (fri (~-e e) env)])
           (cond [(int? v) (int (- (int-n v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (triggered (exception "~: wrong argument type"))]))]
        [(..? e)
         (let ([v1 (fri (..-e1 e) env)]
               [v2 (fri (..-e2 e) env)]) (.. v1 v2)) 
         ]
        [(head? e) (let ([v (fri (head-e e) env)])
                     (cond
                       [(empty? v) (triggered (exception "head: empty sequence"))]
                       [(equal? (fri (?.. v) env) (true)) (..-e1 v)]
                           #|[(equal? (fri (?.. e1) env) (true)) (..-e1 e1)]|#
                           
                           [#t (triggered (exception "head: wrong argument type"))]) ) ]
        [(add? e)
         (let ([v1 (fri (add-e1 e) env)]
               [v2 (fri (add-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2))) ]
                 [(and (true? v1) (true? v2)) (false) ]
                 [(and (true? v1) (false? v2)) (true) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (false) ]
                 [#t (triggered (exception "add: wrong argument type"))]))]
        [(mul? e)
         (let ([v1 (fri (mul-e1 e) env)]
               [v2 (fri (mul-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (int (* (int-n v1) (int-n v2))) ]
                 [(and (true? v1) (true? v2)) (true) ]
                 [(and (true? v1) (false? v2)) (false) ]
                 [(and (false? v1) (true? v2)) (false) ]
                 [(and (false? v1) (false? v2)) (false) ]
                 [#t (triggered (exception "mul: wrong argument type"))]))]
        [(?leq? e)
         (let ([v1 (fri (?leq-e1 e) env)]
               [v2 (fri (?leq-e2 e) env)])
           (cond [(and (int? v1) (int? v2)) (if (<= (int-n v1) (int-n v2)) (true) (false)) ]
                 [(and (true? v1) (true? v2)) (true) ]
                 [(and (true? v1) (false? v2)) (false) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (true) ]
                 [(and (?seq v1) (?seq v2)) (true) ]
                 [#t (triggered (exception "?leq: wrong argument type"))]))]
        [(?all? e)
         (let ([v (fri (?all-e e) env)])
           (cond [(triggered? v) v]
                 [(empty? v) (true)]
                 [(..? v) (if (false? (fri (..-e1 v) env)) (false) (fri (?all (..-e2 v)) env))]
                 [#t (triggered (exception "?all: wrong argument type"))]))]
        [(?any? e)
         (let ([v (fri (?any-e e) env)])
           (cond [(triggered? v) v]
                 [(empty? v) (false)]
                 [(..? v) (if (true? (fri (..-e1 v) env)) (true) (fri (?any (..-e2 v)) env))]
                 [#t (triggered (exception "?any: wrong argument type"))]))]
      
        [(if-then-else? e)
         (let ([v (fri (if-then-else-condition e) env)])
           (if (or (true? v) (and (int? v) (= (int-n v) 1))) (fri (if-then-else-e1 e) env) (fri (if-then-else-e2 e) env)))]
        
        [(handle? e) (let ([v1 (fri (handle-e1 e) env)]
                           [v2 (fri (handle-e2 e) env)]
                           [v3 (handle-e3 e)])
                       (cond [(triggered? v1) v1]
                             [(not (exception? v1)) (triggered (exception "handle: wrong argument type"))]
                             [(and (triggered? v2) (equal? (triggered-e v2) v1) ) (fri v3 env)]
                             [#t v2])) ]
        [(vars? e)
         (let ([v1 (fri (vars-s e) env)]
               [v2 (fri (vars-e1 e) env)]
               [v3 (vars-e2 e)])
           (if (and (list? v1) (list? v2))
               (fri v3 (if (null? env) (map cons v1 (map (lambda (x) (fri x env)) v2)) (append (map cons v1 (map (lambda (x) (fri x env)) v2)) env)))
               (fri v3 (if (null? env) (list (cons v1 (fri v2 env))) (append (list (cons v1 (fri v2 env))) env)))))]
        [#t (error "sintaksa izraza ni pravilna")]))

(define (greater e1 e2)
  (if-then-else (?= e1 e2) (false) (?leq e2 e1)))
(define (rev) 1)
(define (binary) 1)
(define (filtering) 1)
(define (folding) 1)

(define (mapping f seq)
  (fri (call (fun "map" (list "l")
                  (if-then-else (?empty? (valof "l"))
                                (.. (call f (list (valof "l"))) (empty))
                                (add (.. (call f (list (valof "l"))) (empty)) (call (valof "map") (list (valof "l"))))))
             (list (fri seq null)))
       null))