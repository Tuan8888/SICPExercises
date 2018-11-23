;; Exercise 2.56
;; 求导
#lang racket

;; 判断是否为变量
(define (variable? x) (symbol? x))

;; 判断是不是相同变量
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; 判断是否等于某个数字
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; 求和
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;; 求积
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; 判断是否是求和式
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; 判断是否是求积式
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; 判断是否是求幂
(define (exp? x)
  (and (pair? x)
       (eq? (car x) '**)))

;; 获取加数
(define (addend s) (cadr s))
(define (augend s) (caddr s))

;; 获取乘数
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; 求幂
(define (make-exp base exponent)
  (if (or (=number? base 1) (=number? exponent 0))
      1
      (list '** base exponent)))

;; 获取幂的基数、指数
(define (base e) (cadr e))
(define (exponent e) (caddr e))

;; 求导
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exp? exp)
         (make-product (make-product (exponent exp)
                                     (make-exp (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknwon expression type -- DERIV" exp))))

(deriv '(** 2 3) 'x)

