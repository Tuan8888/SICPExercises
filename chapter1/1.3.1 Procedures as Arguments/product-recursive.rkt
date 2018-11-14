;; Exercise 1.31
;; 递归的product
#lang racket

;; 递归的product
(define (product term a next b)
  (if (> a b)
      1
      (* (product term (next a) next b)
         (term a))))

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