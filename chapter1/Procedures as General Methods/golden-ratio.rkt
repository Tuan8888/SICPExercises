;; Exercise 1.35
;; 利用fixed-point计算黄金分割
#lang racket
(require "fixed-point.rkt")

(define (golden-ratio)
  (fixed-point (lambda(x) (+ 1.0 (/ 1 x))) 1))

(golden-ratio)
