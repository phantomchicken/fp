#lang racket


;tok (11 . #procedure)

(define naravna
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define (maptok f tok) ;let* kot v smlu
  (let ([prvi tok])
    (cons (f (car prvi)) (lambda () (maptok f ((cdr prvi))) ;((cdr)) poklice
                           ))))

(define novi (maptok (lambda (x) (+ x 10)) naravna))

;novi
;((cdr novi))

;;(mpair? (mcons 1 3))
;;(mpair? (cons 1 3))

(define naravna2
  (letrec ([f (lambda (x) (mcons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define (razvij tok n)
  (if (zero? n)
      (void) ; na koncu koncamo s tokom
      (if (mpair? (mcdr tok))
          (razvij (mcdr tok) n) ;samo sprehajamo, nastavi mcdr toka na razvij
          (let ([naslednji ((mcdr tok))])
            razvij (begin (set-mcdr! tok naslednji) tok) (- n 1)))));prisli do konca, razvij naprej, poberi naslednji element toka z letom, mcdr proc, ((mcdr)) nasl par

naravna2
(razvij naravna2 5)

naravna2