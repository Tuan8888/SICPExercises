;; Exercise 1.46
;; 利用iterative improve求sqrt
#lang racket

;; 通用迭代improve
(define (iterative-improve good-enough? improve guess)
  (if (good-enough? guess)
      guess
      (iterative-improve good-enough? improve (improve guess))))

;; sqrt
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.0001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2.0))
  (iterative-improve good-enough? improve x))

(sqrt 4)
