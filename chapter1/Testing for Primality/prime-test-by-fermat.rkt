;; Exercise 1.24
;; 通过费马小定理检验是否为质数
#lang racket
(provide (all-defined-out))

;; 求幂的取模
(define (expmod base n m)
  (cond ((= n 0) 1)
        ((odd? n) (remainder (* base (expmod base (- n 1) m)) m))  
        (else (remainder (square (expmod base (/ n 2) m)) m))))

;; 平方
(define (square x)
  (* x x))

;; 判断是否为奇数
(define (odd? n)
  (= (remainder n 2) 1))

;; 一次费马小定理的质数检验
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; 费马小定理检验质数
(define (prime-test-by-fermat-iter n times)
  (cond ((= times 0) true)
        ;; 若一次检验通过，次数减一，再次检验
        ((fermat-test n) (prime-test-by-fermat-iter n (- times 1)))
        (else false)))

;; 费马小定理检验是否为质数，n表示检验次数
(define (prime-test-by-fermat n)
  (prime-test-by-fermat-iter n 10))

