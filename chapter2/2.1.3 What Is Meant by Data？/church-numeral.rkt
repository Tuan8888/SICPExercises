;; Exercise 2.6
;; 丘奇数
#lang racket

;; 0
(define zero (lambda (f) (lambda (x) x)))

;; 加1
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; 1
(define one
  (lambda (f) (lambda (x) (f x))))

;; 2
(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; 加
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))
