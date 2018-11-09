;; Exercise 1.30
;; 迭代版的sigma
#lang racket

(define (sum term a next b)
  (define (sum-iter a res)
    (if (> a b)
        res
        (sum-iter (next a) (+ res (term a)))))
  (sum-iter a 0))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
  
