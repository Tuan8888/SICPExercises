#lang racket
(define (exptIter a b n)
  (if (= n 0) a
      (if (even n) (exptIter a (square b) (/ n 2))
          (exptIter (* a b) b (- n 1)))))

(define (fastExpt b n)
  (exptIter 1 b n))

(define (even n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(fastExpt 3 9)
(fastExpt 5 0)
(fastExpt 2 1)
   
