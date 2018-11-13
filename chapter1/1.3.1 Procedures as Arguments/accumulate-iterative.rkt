;; Exercise 1.32
;; 迭代的accumulate
#lang racket

;; 迭代accumulate
(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a res)
    (if (> a b)
        res
        (accumulate-iter (next a) (combiner res (term a)))))
  (accumulate-iter a null-value))

