;; Exercise1.9
;; 递归加法
#lang racket

;; 减一
(define (dec a)
  (- a 1))

;; 加一
(define (inc a)
  (+ a 1))

;; 递归加法
(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a) b))))