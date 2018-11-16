;; Exercise 2.19
;; 使用list重写count-change
#lang racket

;; 硬币大小
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; 计算找零方式
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; 判断是否没有其余硬币作为选择
(define (no-more? coin-values)
  (null? coin-values))

;; 放弃使用当前硬币
(define (except-first-denomination coin-values)
  (cdr coin-values))

;; 获取当前硬币价值
(define (first-denomination coin-values)
  (car coin-values))

(cc 100 us-coins)
