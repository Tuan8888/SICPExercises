;; Exercise 2.46
;; 向量
#lang racket
(provide (all-defined-out))

;; make
(define (make-vect x y)
  (cons x y))

;; selector
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

;; 加
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

;; 减
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

;; 乘系数
(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))


    
