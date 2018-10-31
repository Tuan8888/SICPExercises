;; Exercise 1.3
;; 返回三个数中较大两个数的平方和
#lang racket

;; 返回两个输入的较小数
(define (min a b)
  (if (< a b)
      a
      b))

;; 返回三个数中最小的一个
(define (min-three a b c)
  (min (min a b)
       (min b c)))

;; 求平方
(define (square x)
  (* x x))

;; 返回三个数中较大两个数的平方和
;; 先计算三个数的平方和，再减去最小数的平方
(define (two-larger-square-sum a b c)
  (- (+ (square a) (square b) (square c))
     (square (min-three a b c))))

(two-larger-square-sum 1 2 3)
(two-larger-square-sum 6 5 7)
(two-larger-square-sum 9 2 8)
(two-larger-square-sum 5 2 7)
