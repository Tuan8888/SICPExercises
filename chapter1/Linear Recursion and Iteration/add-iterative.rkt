;; Exercise1.9
;; 循环加法
#lang racket

;; 减一
(define (dec a)
  (- a 1))

;; 加一
(define (inc a)
  (+ a 1))

;; 循环加法
(define (add a b)
  (if (= a 0)
      b
      (add (dec a) (inc b))))

(add 4 8)
(add 0 8)
(add 4 0)
