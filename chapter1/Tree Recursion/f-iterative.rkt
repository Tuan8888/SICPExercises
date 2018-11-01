;; Exercise 1.11
;; 迭代求f函数值
#lang racket

;; 迭代求f
(define (f-iterative f1 f2 f3 n)
  (cond ((< n 3) n)
        ((= n 3) f1)
        (else (f-iterative (+ f1 (* 2 f2) (* 3 f3)) f1 f2 (- n 1)))))

;; 求f
(define (f n)
  (f-iterative 4 2 1 n))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
