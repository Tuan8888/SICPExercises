;; Exercise 2.7~2.12
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

;; 百分比表示，make
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

;; 百分比表示，selector
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (/ (- (upper-bound i) (center i)) (center i)))

;; print
(define (print-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

;; 以百分比误差的形式打印
(define (print-center-percent-interval i)
  (newline)
  (display "center = ")
  (display (center i))
  (display ", percent = ")
  (display (percent i)))

