;; Exercise 1.17
;; 快速乘法
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
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ;; 处理b为负的情况
        ((< b 0) (- 0 (fast-mult a (- 0 b))))
        ((even b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 3 0)
(fast-mult 3 1)
(fast-mult 2 5)
(fast-mult 0 8)
(fast-mult -2 3)
(fast-mult 3 -9)
