;; Exercise 1.11
;; 递归求f函数值
#lang racket

;; 递归求f
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
