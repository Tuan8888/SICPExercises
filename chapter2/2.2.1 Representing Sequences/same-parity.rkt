;; Exercise 2.20
;; 返回与第一个元素奇偶性一致的元素
#lang racket
(require "list.rkt")

;; 判断奇偶性一致
(define (same? x y)
  (= (remainder x 2) (remainder y 2)))

;; 过滤
(define (same-parity x . l)
  (define (same-parity-iter remain result)
    (if (null? remain)
        (cons x (reverse result))
        (if (same? x (car remain))
            (same-parity-iter (cdr remain) (cons (car remain) result))
            (same-parity-iter (cdr remain) result))))
  (same-parity-iter l null))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
