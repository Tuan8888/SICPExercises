;; Exercise 2.56~2.58
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
        (else (list a1 '+ a2))))

;; 求积
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; 判断是否是求和式
(define (sum? x)
  (contains? x '+))

;; 判断是否是求积式
(define (product? x)
  (and (not (contains? x '+)) (contains? x '*)))

;; 判断list中是否含有某元素
(define (contains? l item)
  (cond ((null? l) #f)
        ((eq? (car l) item) #t)
        (else (contains? (cdr l) item))))

;; 判断是否是求幂
(define (exp? x)
  (and (pair? x)
       (eq? (car x) '**)))

;; 从list中获取指定item之前的部分
(define (get-before l item)
  (define (iter remain)
    (if (or (null? remain) (eq? (car remain) item))
        null
        (cons (car remain) (iter (cdr remain)))))
  (remove-brackets-if-only-one (iter l)))

;; 从list中获取指定item之后的部分
(define (get-after l item)
  (define (iter remain)
    (cond ((null? remain) null)
          ((eq? (car remain) item) (cdr remain))
          (else (iter (cdr remain)))))
  (remove-brackets-if-only-one (iter l)))

;; 若list中只含有一个元素，去除括号
(define (remove-brackets-if-only-one l)
  (if (null? (cdr l))
      (car l)
      l))

;; 获取加数
(define (addend s) (get-before s '+))
(define (augend s) (get-after s '+))

;; 获取乘数
(define (multiplier p) (car p))
(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest))
        (car rest)
        rest)))

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

(deriv '(3 * x + 5 * x * x + (10 + 7 * x)) 'x)


