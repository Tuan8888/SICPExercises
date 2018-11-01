;; Exercise 1.28
;; 通过Miller-Rabin测试检验是否为质数
#lang racket

;; 求幂的取模，若检测到某次迭代幂模为1，返回0，其余情况返回幂的取模结果
(define (expmod base n m)
  (cond ((= n 0) 1)
        ((odd? n) (remainder (* base (expmod base (- n 1) m)) m))  
        (else (remainder (square-and-check (expmod base (/ n 2) m) m) m))))

;; 平方，检测平方结果对m的模，为1则返回0，其余情况返回平方结果
(define (square-and-check x m)
  (define res (* x x))
  (cond ((or (= x 1) (= x (- m 1))) res)
        ((= (remainder res m) 1) 0)
        (else res)))

;; 判断是否为奇数
(define (odd? n)
  (= (remainder n 2) 1))

;; 一次质数检验
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; 质数检验
(define (prime-test-by-miller-rabin-iter n times)
  (cond ((= times 0) true)
        ;; 若一次检验通过，次数减一，再次检验
        ((fermat-test n) (prime-test-by-miller-rabin-iter n (- times 1)))
        (else false)))

;; 检验是否为质数，n表示检验次数
(define (prime-test-by-miller-rabin n)
  (prime-test-by-miller-rabin-iter n 10))

(prime-test-by-miller-rabin 561)
(prime-test-by-miller-rabin 1105)
(prime-test-by-miller-rabin 1729)
(prime-test-by-miller-rabin 2465)
(prime-test-by-miller-rabin 2821)
(prime-test-by-miller-rabin 6601)
(prime-test-by-miller-rabin 3)
(prime-test-by-miller-rabin 5)
(prime-test-by-miller-rabin 48)
(prime-test-by-miller-rabin 49)
