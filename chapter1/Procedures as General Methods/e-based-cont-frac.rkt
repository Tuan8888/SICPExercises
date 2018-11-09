;; Exercise 1.38
;; 计算e-2
#lang racket
(require "cont-frac-iterative.rkt")

;; 计算d，规律为121,141,161
(define (d x)
  (if (< (remainder x 3) 2)
      1.0
      (* 2.0 (/ (+ x 1) 3))))

;; 计算e-2
(cont-frac-iterative d
                     (lambda(x) 1.0)
                     10)

