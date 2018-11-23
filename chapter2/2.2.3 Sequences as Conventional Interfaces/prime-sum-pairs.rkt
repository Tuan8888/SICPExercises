;; Exercise 2.40~2.41
;; 找出所有和为质数的整数对
#lang racket
(require math/number-theory)
(require "sequence-operations.rkt")
(provide (all-defined-out))

;; 展开list，再map
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;; 判断整数对的和是否为质数
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; 整数对和质数拼接
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; 枚举区间内所有整数
(define (enumerate-interval x y)
  (define (iter n)
    (if (> n y)
        null
        (cons n (iter (+ n 1)))))
  (iter x))
      
;; 生成整数对，并过滤和不为质数的部分
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; 产生1<= j< i<= n的整数对
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda(j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; 产生1<=k<j<i<=n的整数组
(define (unique-triples n)
  (flatmap
   (lambda(i) (map
               (lambda(x)
                 (cons i x))
               (unique-pairs (- i 1))))
   (enumerate-interval 1 n)))

;; 判断整数组的和是否等于s
(define (sum-equal? s)
  (lambda(t) (= (accumulate + 0 t) s)))

;; 寻找n以内的和为s的整数组
(define (triple-sum s n)
  (filter (sum-equal? s) (unique-triples n)))
