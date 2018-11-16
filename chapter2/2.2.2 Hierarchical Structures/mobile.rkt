;; Exercise 2.29
;; mobile
#lang racket

;; make mobile
(define (make-mobile left right)
  (list left right))

;; make branch
(define (make-branch length structure)
  (list length structure))

;; mobile selector
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))

;; branch selector
(define (branch-length m)
  (car m))
(define (branch-structure m)
  (car (cdr m)))

;; 判断branch是否包含mobile
(define (branch-contains-mobile b)
  (pair? (branch-structure b)))

;; 判断branch的总重
(define (total-weight-branch b)
  (cond ((null? b) 0)
        ((branch-contains-mobile b) (total-weight (branch-structure b)))
        (else (branch-structure b))))

;; mobile的总重
(define (total-weight m)
  (cond ((null? m) 0)
        ((pair? m) (+ (total-weight-branch (left-branch m))
                      (total-weight-branch (right-branch m))))
        (else m)))

;; 计算branch产生的力矩
(define (torque b)
  (* (branch-length b) (total-weight-branch b)))

;; 判断branch是否平衡
(define (balance-branch b)
  (if (branch-contains-mobile b)
      (balance (branch-structure b))
      true))

;; 判断mobile是否平衡
(define (balance m)
  (if (null? m)
      true
      (and (= (torque (left-branch m)) (torque (right-branch m)))
           (balance-branch (left-branch m))
           (balance-branch (right-branch m)))))

(define b1 (make-branch 1 3))
(define b2 (make-branch 3 1))
(define m1 (make-mobile b1 b2))
(define b3 (make-branch 1 m1))
(define b4 (make-branch 1 4))
(define m2 (make-mobile b3 b4))

(balance m2)
