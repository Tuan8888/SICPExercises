;; Exercise 2.1
;; 有理数
#lang racket

;; 有理数定义
(define (numer x) (car x))
(define (demon x) (cdr x))
;; 处理分子分母均为正的有理数
(define (make-positive-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
;; 处理正负有理数
(define (make-rat n d)
  (define positive-rat (make-positive-rat (abs n) (abs d)))
  (if (< (* n d) 0)
      (cons (- (numer positive-rat)) (demon positive-rat))
      positive-rat))

;; 打印有理数
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (demon x)))

;; 有理数加法
(define (add-rat x y)
  (make-rat (+ (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

;; 有理数减法
(define (sub-rat x y)
  (make-rat (- (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

;; 有理数乘法
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (demon x) (demon y))))

;; 有理数除法
(define (div-rat x y)
  (make-rat (* (numer x) (demon y))
            (* (demon x) (numer y))))

;; 判断有理数是否相等
(define (equal-rat? x y)
  (= (* (numer x) (demon y))
     (* (numer y) (demon x))))

(define rat1 (make-rat 3 -4))
(define rat2 (make-rat -9 7))
(define rat3 (make-rat -9 -7))
(define rat4 (make-rat 9 7))
(print-rat rat1)
(print-rat rat2)
(print-rat rat3)
(print-rat rat4)


