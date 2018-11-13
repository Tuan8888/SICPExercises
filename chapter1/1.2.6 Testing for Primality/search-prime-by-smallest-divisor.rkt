;; Exercise 1.22
;; 搜索给定范围的质数，并打印时间
;; 判断质数使用最小因子法
#lang racket
(require "prime-test-by-smallest-divisor.rkt")

;; 平方
(define (square x)
  (* x x))

;; 搜索指定数量和范围的质数并打印
(define (start-prime-test n start-time number)
  (cond ((= number 0) (report-prime (- (current-inexact-milliseconds) start-time)))
        ((prime-test-by-smallest-divisor n)
         (display n)
         (display " ")
         (start-prime-test (+ n 1) start-time (- number 1)))
        (else (start-prime-test (+ n 1) start-time number))))

;; 打印程序耗费时间
(define (report-prime elapsed-time)
  (display "*** ")
  (display elapsed-time)
  (newline))

;; 开始质数搜索，并记录当前时间
(define (searchForPrimes startNumber number)
  (start-prime-test startNumber (current-inexact-milliseconds) number))      
;; 求幂的模
(define (expmod base n m)
  (cond ((= n 0) 1)
        ((odd? n) (remainder (* base (expmod base (- n 1) m)) m))  
        (else (remainder (square (expmod base (/ n 2) m)) m))))

(searchForPrimes 10000000000 3)
(searchForPrimes 100000000000 3)
(searchForPrimes 1000000000000 3)
(searchForPrimes 10000000000000 3)
(searchForPrimes 100000000000000 3)
