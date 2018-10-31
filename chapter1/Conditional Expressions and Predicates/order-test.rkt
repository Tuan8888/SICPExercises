;; Exercise 1.5
#lang lazy

;; 定义一个死循环递归
(define (p) (p))

;; 检测解释器是应用序还是正则序
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
