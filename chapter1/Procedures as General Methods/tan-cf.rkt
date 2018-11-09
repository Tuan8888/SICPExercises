;; Exercise 1.39
;; 近似计算tan
#lang racket
(require "cont-frac-iterative.rkt")

;; 计算近似tan
(define (tan-cf x k)
  (/ (cont-frac-iterative (lambda(y) (- (* y 2) 1.0))
                       (lambda(y) (- (* x x)))
                       k)
     (- x)))

(tan-cf 1 30)
(tan-cf 1.5 30)
(tan-cf 2 30)

