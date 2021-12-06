#lang racket
(define (my-delay thunk)
  (mcons #f thunk))

(define (my-force prom)
    (if (mcar prom)
        (mcdr prom)
        (begin (set-mcar! prom #t)
            (set-mcdr! prom ((mcdr prom)))
            (mcdr prom))))