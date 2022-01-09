#lang racket
(require "02-project.rkt")

(require rackunit)
(require rackunit/text-ui)

(define all-tests
  (test-suite
   "all"
   (test-suite
    "pulic"
    (test-case
     "add1"
     (check-equal?
      (add (mul (true) (true)) (false))
      (add (mul (true) (true)) (false))))
 
    (test-case
     "add2"
     (check-equal?
      (fri (add (mul (true) (true)) (false)) null)
      (true)))

    (test-case
     "add3"
     (check-equal?
      (fri (add (add (int 9) (int 9)) (true)) null)
      (triggered (exception "add: wrong argument type"))))

    (test-case
     "handle1"
     (check-equal?
      (fri (handle (exception "add: wrong argument type")
                   (add (add (int 9) (int 9)) (true))
                   (false))
           null)
      (false)))
    (test-case
     "handle2"
     (check-equal?
      (fri (handle (exception "fatal error")
                   (add (add (int 9) (int 9)) (true))
                   (false))
           null)
      (triggered (exception "add: wrong argument type"))))

    (test-case
     "handle3"
     (check-equal?
      (fri (handle (exception "fatal error")
                   (add (add (int 9) (int 9)) (int -1))
                   (false))
           null)
      (int 17)))

    (test-case
     "handle4"
     (check-equal? 
      (fri (handle (int 1337)
                   (add (add (int 9) (int 9)) (int -1))
                   (false))
           null)
      (triggered (exception "handle: wrong argument type"))))

    (test-case
     "handle5"
     (check-equal? 
      (fri (handle (trigger (exception "fatal error"))
                   (add (add (int 9) (int 9)) (int -1))
                   (false))
           null)
      (triggered (exception "fatal error"))))

    (test-case
     "handle6"
     (check-equal?
      (fri (handle (head (.. (exception "error") (int 10)))
                   (exception "error") (int 2)) null)
      (exception "error")))

    (test-case
     "handle7"
     (check-equal?  (fri (handle (exception "error") (trigger (exception "error")) (int 2)) null)
                    (int 2)))
    (test-case
     "handle8"
     (check-equal?  (fri (handle (exception "error2")
                                 (handle (exception "error1") (trigger (exception "error2")) (int 2))
                                 (int 1)) null)
                    (int 1)))

    (test-case
     "add4"
     (check-equal? 
      (fri (add (int 1) (trigger (exception "fatal error"))) null)
      (triggered (exception "fatal error"))))

    (test-case
     "triggered1"
     (check-equal? 
      (fri (trigger (exception "fatal error")) null)
      (triggered (exception "fatal error"))))
    
    (test-case
     "seq1"
     (check-equal?
      (?seq (.. (int 1) (.. (int 2) (empty))))
      (?seq (.. (int 1) (.. (int 2) (empty))))))
    
    (test-case
     "seq2"
     (check-equal?
      (fri (.. (?seq (.. (int 1) (.. (int 2) (empty))))
               (?seq (.. (int 1) (.. (int 2) (int 3))))) null)
      (.. (true) (false))))
 
    (test-case
     "vars1"
     (check-equal?
      (fri (vars "a" (add (mul (int 1) (int 2)) (mul (int -3) (int 4)))
                 (mul (valof "a") (valof "a"))) null)
      (int 100)))
    
    (test-case
     "vars2"
     (check-equal?
      (fri (vars (list "a" "b")
                 (list (mul (mul (int 1) (int 2)) (mul (int -3) (int 4)))
                       (~ (add (mul (int 1) (int 2)) (mul (int -3) (int 4)))))
                 (add (valof "a") (valof "b"))) null)
      (int -14)))
    
    (test-case
     "fib1"
     (check-equal?
      (fri (call (fun "fib" (list "n")
                      (if-then-else (?leq (valof "n") (int 2))
                                    (int 1) (add (call (valof "fib")
                                                       (list (add (valof "n") (int -1))))
                                                 (call (valof "fib")
                                                       (list (add (valof "n") (int -2)))))))
                 (list (int 10))) null)
      (int 55)))
    
    (test-case "all" (check-equal?
                      (fri (?all (.. (true)
                                     (.. (?leq (false) (true))
                                         (.. (?= (.. (int -19) (int 0))
                                                 (.. (head
                                                      (tail
                                                       (tail (add (.. (int 1) (empty)) (.. (int 5) (.. (int -19) (empty)))))))
                                                     (int 0)))
                                             (empty)))))
                           null)
                      (true)))
    
    (test-case "vars3" (check-equal?
                        (fri (vars (list "s1" "s2" "s3")
                                   (list (.. (false) (true))
                                         (.. (int 1) (int 2))
                                         (.. (int 4) (int 4)))
                                   (mul (valof "s1") (mul (valof "s2") (valof "s3")))) null)
                        (triggered (exception "mul: wrong argument type"))))
    
    (test-case "variables1" (check-equal?
                             (fri (vars (list "a" "b" "c")
                                        (list (int 1) (int 2) (int 3))
                                        (fun "linear" (list "x1" "x2" "x3")
                                             (add (mul (valof "a") (valof "x1"))
                                                  (add (mul (valof "b") (valof "x2"))
                                                       (mul (valof "c") (valof "x3")))))) null)
                             (closure (list (cons "a" (int 1))(cons "b" (int 2)) (cons "c" (int 3)))
                                      (fun "linear" '("x1" "x2" "x3")
                                           (add (mul (valof "a") (valof "x1"))
                                                (add (mul (valof "b") (valof "x2"))
                                                     (mul (valof "c") (valof "x3")))))))))
   
   (test-suite
    "misc"
    (test-case "add-seq" (check-equal?
                          (fri (add (.. (false) (empty))
                                    (.. (int 3) (empty))) null)
                          (.. (false) (.. (int 3) (empty)))))
  
    (test-case "add-empty" (check-equal?
                            (fri (add (empty) (empty)) null)
                            (empty))))
   
   (test-case
    "long-long"
    (check-equal?
     (fri
      (vars "a" (int 10)
            (vars (list "f" "g")
                  (list (fun "" (list "a" "b")
                             (add (valof "a") (mul (int 5) (valof "b"))))
                        (fun "" (list "c")
                             (add (valof "a") (valof "c"))))
                  (vars (list "a" "d" "g" "e")
                        (list (int 1)
                              (call (valof "g") (list (int -9)))
                              (fun "" (list "x")
                                   (add (valof "a") (mul (valof "x")
                                                         (call (valof "f")
                                                               (list (int 1) (valof "a"))))))
                              (fun "" (list "f" "x")
                                   (call (valof "f") (list (valof "x")))))
                        (vars (list "fib" "test" "unit-fun" "proc")
                              (list (fun "fib" (list "n")
                                         (if-then-else (?leq (valof "n") (int 2))
                                                       (int 1)
                                                       (add (call (valof "fib")
                                                                  (list (add (valof "n")
                                                                             (int -1))))
                                                            (call (valof "fib")
                                                                  (list (add (valof "n")
                                                                             (int -2)))))))
                                    (fun "" (list "x")
                                         (add (valof "x") (int 2)))
                                  
                                    (fun "" null
                                         (add (add (valof "a")
                                                   (valof "a"))
                                              (valof "a")))
                                  
                                    (proc ""
                                          (folding
                                           (fun "" (list "x" "acc") (mul (valof "x") (valof "acc")))
                                           (int 1)
                                           (.. (valof "a")
                                               (.. (int 2)
                                                   (.. (int 3)
                                                       (.. (int 4)
                                                           (.. (call (valof "g")
                                                                     (list (int 5)))
                                                               (empty)))))))))
                              
                              
                              (.. (call (valof "unit-fun") null)
                                  (.. (call (valof "proc") null)
                                      (add (call (valof "g")
                                                 (list (add (int 5)
                                                            (call (valof "test")
                                                                  (list (int 3))))))
                                           (add (valof "d")
                                                (add (call (valof "f")
                                                           (list (int -1) (int -2)))
                                                     (add (valof "a")
                                                          (add (call (valof "fib")
                                                                     (list (int 5)))
                                                               (call (valof "e")
                                                                     (list (valof "test") (int 3))))))))))))))
      null)
     (.. (int 3) (.. (int 6360) (int 521)))))))

(run-tests all-tests)