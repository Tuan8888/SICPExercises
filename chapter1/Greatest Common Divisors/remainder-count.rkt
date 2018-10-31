;; Exercise 1.20
;; 计算欧几里得算法调用remainder函数次数
#lang racket

;; 重新定义remainder函数，每次调用都打出一个yes
(define (remainder-count a b)
  (display "yes\n")
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder-count a b))))

(gcd 206 40)
