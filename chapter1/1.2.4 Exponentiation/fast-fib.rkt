;; Exercise 1.19
;; 快速求解裴波那契数
#lang racket

;; 求解裴波那契数
(define (fast-fib n)
  (fib-iter 1 0 0 1 n))

;; 迭代求解
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; 平方
(define (square x)
  (* x x))

;; 判断是否为偶数
(define (even? x)
  (= (remainder x 2) 0))

(fast-fib 0)
(fast-fib 1)
(fast-fib 2)
(fast-fib 3)
(fast-fib 4)
(fast-fib 5)
(fast-fib 6)
