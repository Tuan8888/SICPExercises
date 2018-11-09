;; 积分计算
#lang racket
(provide (all-defined-out))
(require "sum.rkt")
(require "cube.rkt")

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
