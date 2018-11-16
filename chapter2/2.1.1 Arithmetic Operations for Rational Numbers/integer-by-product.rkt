;; Exercise 2.5
;; 用2^a3^b这个整数，记录a、b
#lang racket

;; cons
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; 判断是否为偶数
(define (even? x)
  (=  (remainder x 2) 0))

;; 获取x中因子a的个数
(define (get-factor-num x a)
  (define (iter n r)
    (if (= (remainder r a) 0)
        (iter (+ n 1) (/ r a))
        n))
  (iter 0 x))
      
;; selector
(define (car c)
  (get-factor-num c 2))
(define (cdr c)
  (get-factor-num c 3))

(define c (cons 2 5))
(car c)
(cdr c)
