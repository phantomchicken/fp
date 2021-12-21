#lang racket
(provide (all-defined-out))
;1 (car ((cdr ((cdr ones)))))
(define ones
  (cons 1 (lambda () ones)))

;2 (car ((cdr ((cdr naturals)))))
(define naturals
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

;3 (car ((cdr ((cdr fibs)))))
(define fibs
  (letrec
     ((fib (lambda (x y) (cons x (lambda () (fib y (+ x y)))))))
               (fib 1 1)))

;4 (first 5 fibs)
(define (first n tok)
  (if (> n 1) 
      (begin
        (displayln (car tok))
        (first (- n 1) ((cdr tok))))
      (displayln (car tok))))

;5 (first 5 (squares fibs))
(define (squares tok)
  (letrec
      ([f (lambda (e)
            (cons (* (car e) (car e)) ;first ni mozen ker funkcija 4 ga zasenci
                  (lambda () (f ((cdr e))))))])
    (f tok)))

;6 (sml nil) (sml null (sml nil)) (sml hd (sml 5 :: null)) (sml tl (sml 5 :: (sml 4 :: (sml nil))))
(define-syntax sml
  (syntax-rules ()
    [(sml nil) '()]
    [(sml null e) (null? e)]
    [(sml e1 :: e2) (cons e1 e2) ]
    [(sml hd e) (first e)]
    [(sml tl e) (rest e)]
    ))

;7 (define f (my-delay (lambda () (begin (write "bla") 123)))) (my-force f)
(define (my-delay thunk)
  (mcons #f thunk))
;; (sproži prom)  `prom` mora biti mutiran par,
;;  ki je rezultat klica funcije `zakasni`.

(define (my-force prom)
  (if (mcar prom) ;ali je izracunan
      (mcdr prom) ;ce je, vrni kar vrednost
      (begin (set-mcar! prom #t) ; begin - zaporedje ukazov, nastavi da je izracunano
             (set-mcdr! prom ((mcdr prom))) ;izracunaj drugi del
             (mcdr prom)))) ;vrni izracun

;8 (partitions 3 7)
(define (partitions k n)
  n
  )