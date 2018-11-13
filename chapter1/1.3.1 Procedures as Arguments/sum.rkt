;; sigma计算
#lang racket
(provide (all-defined-out))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
