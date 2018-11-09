;; Exercise 1.33
;; 带filter的accumulate，迭代计算
#lang racket
(provide (all-defined-out))

;; 迭代filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (accumulate-iter a res)
    (cond ((> a b) res)
          ((filter? a) (accumulate-iter (next a) (combiner res (term a))))
          (else (accumulate-iter (next a) res))))
  (accumulate-iter a null-value))
