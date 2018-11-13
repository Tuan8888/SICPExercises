;; Exercise 1.31
;; 迭代的product
#lang racket

;; 迭代的product
(define (product term a next b)
  (define (product-iter a res)
    (if (> a b)
        res
        (product-iter (next a) (* res (term a)))))
  (product-iter a 1))

;; 判断是否为偶数
(define (even? x)
  (= (remainder x 2) 0))

;; 计算pi
(define (pi-product n)
  (define (next k)
    (+ k 1))
  (define (term k)
    (/ (if (even? k)
           (+ k 2.0)
           (+ k 1.0))
       (if (even? k)
           (+ k 1.0)
           (+ k 2.0))))
  (* (product term 1 next n)
     4))

(pi-product 10000000)
