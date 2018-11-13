;; Exercise 1.24
;; 通过费马小定理检验质数法，搜索给定范围内的质数
#lang racket
(require "prime-test-by-fermat.rkt")

;; 平方
(define (square x)
  (* x x))

;; 搜索指定数量和范围的质数并打印
(define (start-prime-test n start-time number)
  (cond ((= number 0) (report-prime (- (current-inexact-milliseconds) start-time)))
        ((prime-test-by-fermat n)
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

(searchForPrimes 1000000 3000)
(searchForPrimes 100000000 3000)
