;; Exercise 2.7~2.11
;; 区间计算
#lang racket

;; 加
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; 减
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 乘
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (cond ((right-zero? x)
           (cond ((right-zero? y) (make-interval p1 p4))
                 ((span-zero? y)  (make-interval p3 p4))
                 (else (make-interval p3 p2))))
          ((span-zero? x)
           (cond ((right-zero? y) (make-interval p2 p4))
                 ((span-zero? y)  (make-interval (min p2 p3) (max p1 p4)))
                 (else (make-interval p3 p1))))
          (else
           (cond ((right-zero? y) (make-interval p2 p3))
                 ((span-zero? y)  (make-interval p2 p1))
                 (else (make-interval p4 p1)))))))

;; 除
(define (div-interval x y)
  (if (span-zero? y)
      (error "the interval spans zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; 检测区间是否跨过0，包含端点在0的情况
(define (span-zero? i)
  (and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))

;; 判断区间是否在0左侧
(define (left-zero? i)
  (and (< (lower-bound i) 0) (< (upper-bound i) 0)))

;; 判断区间是否在0右侧
(define (right-zero? i)
  (and (> (lower-bound i) 0) (> (upper-bound i) 0)))

;; make
(define (make-interval a b) (cons a b))

;; selector
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))

;; width
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i))))

;; print
(define (print-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

;; 百分比表示，make
(define (make-center-percent center percent)
  (make-interval (- center (* center percent)) (+ center (* center percent))))

;; 百分比表示，selector
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (/ (- (upper-bound i) center) center))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define i1 (make-interval 2 5))
(define i2 (make-interval 3 7))
(par1 i1 i2)
(par2 i1 i2)
