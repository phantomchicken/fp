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
        [(..? e)
         (let ([v1 (fri (..-e1 e) env)]
               [v2 (fri (..-e2 e) env)]) (.. v1 v2)) 
         ]
        [(valof? e) (let ([v (fri (valof-s e) env)])
                      (cond [(triggered? v) v]
                            [(assoc v env) (cdr (assoc v (reverse env)))]
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
                           [(empty? v) (true)]
                           [(..? v) (fri (?seq (..-e2 v)) env) ]
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
                        (cond [(triggered? exn) exn]
                              [(exception? exn) (triggered exn)]
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
                       [(equal? (fri (?.. v) env) (true)) (..-e1 v)]                          
                       [#t (triggered (exception "head: wrong argument type"))]) ) ]
        [(tail? e) (let ([v (fri (tail-e e) env)])
                     (cond [(triggered? v) v]
                       [(empty? v) (triggered (exception "tail: empty sequence"))]
                       [(equal? (fri (?.. v) env) (true)) (..-e2 v)]
                       [#t (triggered (exception "tail: wrong argument type"))]) ) ]
        [(add? e)
         (let ([v1 (fri (add-e1 e) env)]
               [v2 (fri (add-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(empty? v1) v2]
                 [(empty? v2) v1]
                 [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2))) ]
                 [(and (true? v1) (true? v2)) (false) ]
                 [(and (true? v1) (false? v2)) (true) ]
                 [(and (false? v1) (true? v2)) (true) ]
                 [(and (false? v1) (false? v2)) (false) ]
                 
                 [(and (equal? (fri (?seq v1) env) (true)) (equal? (fri (?seq v2) env) (true))) (concat v1 v2 env)] 
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
                 [(and (equal? (fri (?seq v1) env) (true)) (equal? (fri (?seq v2) env) (true))) (if (<= (len v1 env) (len v2 env)) (true) (false)) ]
                 [#t (triggered (exception "?leq: wrong argument type"))]))]
        [(?all? e)
         (let ([v (fri (?all-e e) env)])
           (cond [(triggered? v) v]
                 [(empty? v) (true)]
                 [(..? v) (if (onlybools (..-e2 v)) (if (false? (fri (..-e1 v) env)) (false)  (fri (?all (..-e2 v)) env)  ) (triggered (exception "?all: wrong argument type")))  ]
                 [#t (triggered (exception "?all: wrong argument type"))]))]
        [(?any? e)
         (let ([v (fri (?any-e e) env)])
           (cond [(triggered? v) v]
                 [(empty? v) (false)]
                 [(..? v) (if (onlybools (..-e2 v)) (if (true? (fri (..-e1 v) env)) (true)  (fri (?all (..-e2 v)) env)  ) (triggered (exception "?any: wrong argument type")))  ]
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
        [(vars? e) (let ([s (fri (vars-s e) env)]
                         [e1 (fri (vars-e1 e) env)]
                         [e2 (vars-e2 e)])
                     (cond [(triggered? s) s]
                           [(triggered? e1) e1]
                           [(or (null? s) (null? e1)) (fri e2 env)]
                           [(and (list? s) (list? e1)) (fri e2 (append env (concatenv s e1 env null)))]       ;(if (triggered? (fri (car e1) env)) (fri (car e1) env) (fri (vars (cdr s) (cdr e1) e2) (append env (list (cons (fri (car s) env) (fri (car e1) env))))))]
                           [#t (fri e2 (append-test env (list (cons s e1))))]))]
               
        [(fun? e) (closure env e)]
        [(proc? e) (closure env e)]
        [(closure? e) e]        
        [(call? e) (let ([ok (fri (call-e e) env)]
                         [args (call-args e)])
                     (cond [(triggered? ok) ok]
                           [(proc? ok) (fri (proc-body ok) (append-test env (list (cons (proc-name ok) ok))))]
                           [(closure? ok)
                            (let ([c-f (closure-f ok)])
                            (fri (vars (fun-farg c-f) args (fun-body c-f)) (append-test env (list (cons (fun-name c-f) ok)))))]
                           [#t (triggered (exception "call: wrong argument type"))]))]
                   
        [#t (error "sintaksa izraza ni pravilna")]))

(define (concatenv s e1 env novi-env)
    (cond [(or (null? s) (null? e1)) novi-env]
          [(and (list? s) (list? e1))
           (let ([car-s (car s)]
                 [car-e1 (car e1)]
                 [cdr-s (cdr s)]
                 [cdr-e1 (cdr e1)])
           (if (triggered? (fri car-e1 env)) (fri car-e1 env) (concatenv cdr-s cdr-e1 env (append novi-env (list (cons (fri car-s env) (fri car-e1 env)))))))]))

(define (concat xs ys env)
           (cond [(and (empty? xs) (empty? ys)) (empty)]
                 [(and (empty? xs) (..? ys)) ys] 
                 [(and (..? xs) (empty? ys)) xs] 
                 [(and (..? xs) (..? ys)) (fri (.. (fri (head xs) env) (concat (fri (tail xs) env) ys env)) env)]
                 
           ))

(define (append-test lhs rhs)
  (if (null? lhs)
      rhs
      (cons (first lhs) (append-test (rest lhs) rhs))))

(define (len xs env)
     (cond [(empty? xs) 0]
           [(..? xs) (+ 1 (len (..-e2 xs) env))]
           [#t 0]))

(define (onlybools xs)
  (cond [(empty? xs) #t]
        [(equal? (head xs) (true)) (onlybools (tail xs)) ]
        [(equal? (head xs) (false)) (onlybools (tail xs)) ]
        [#t #f]))

(define (greater e1 e2)
  (if-then-else (?= e1 e2) (false) (?leq e2 e1)))
(define (rev) 1)
(define (binary) 1)
(define (filtering f seq)
  (fri (call (fun "filter" (list "l")
                  (if-then-else (?empty? (valof "l"))
                                (.. (call f (list (valof "l"))) (empty))
                                (add (.. (valof "l") (empty)) (call (valof "filter") (list (valof "l"))))))
             (list (fri seq null)))
       null))
(define (folding) 1)

(define (mapping f seq)
  (fri (call (fun "map" (list "l")
                  (if-then-else (?empty? (valof "l"))
                                (.. (call f (list (head (valof "l"))) (empty)))
                                (add (.. (call f (list (valof "l"))) (empty)) (call (valof "map") (list (valof "l"))))))
             (list (fri seq null)))
       null))

