;; Exercise 1.32
;; 递归的accumulate
#lang racket

;; 递归accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate combiner null-value term (next a) next b)
                (term a))))

