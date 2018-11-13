;; Exercise 1.18
;; 迭代快速乘法
#lang racket

;; 倍增
(define (double x)
  (+ x x))

;; 减半
(define (halve x)
  (/ x 2))

;; 检测是否为偶数
(define (even x)
  (= (remainder x 2) 0))

;; 快速乘法
(define (fast-mult-iterative a b)
  (cond ((= b 0) 0)
        ;; 处理b为负的情况
        ((< b 0) (- 0 (fast-mult-iterative a (- 0 b))))
        ;; 改递归为迭代
        ((even b) (fast-mult-iterative (double a) (halve b)))
        (else (+ a (fast-mult-iterative a (- b 1))))))

(fast-mult-iterative 3 0)
(fast-mult-iterative 3 1)
(fast-mult-iterative 2 5)
(fast-mult-iterative 0 8)
(fast-mult-iterative -2 3)
(fast-mult-iterative 3 -9)
