;; 有理数包
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "apply-generic.rkt")
(provide (all-defined-out))

;; 安装有理数包
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (apply-generic 'reduce n d))
  ;; 计算
  (define (add x y) (apply-generic 'add x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; 判断有理数是否相等
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  ;; 判断有理数是否为零
  (define (rational-zero? r)
    (= (numer r) 0))
  ;; 升级为实数
  (define (raise-rational r)
    ((get 'make 'real) (/ (numer r) (denom r))))
  ;; 降级为整数
  (define (project-rational r)
    ((get 'make 'integer) (round (/ (numer r) (denom r)))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'sin '(rational)
       (lambda (r) (tag (sin (/ (numer r) (denom r))))))
  (put 'cos '(rational)
       (lambda (r) (tag (cos (/ (numer r) (denom r))))))
  (put 'atan '(rational)
       (lambda (r) (tag (atan (/ (numer r) (denom r))))))
  (put 'sqrt '(rational)
       (lambda (r) (tag (sqrt (/ (numer r) (denom r))))))
  (put 'square '(rational)
       (lambda (r) (tag (*
                         (/ (numer r) (denom r))
                         (/ (numer r) (denom r))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put 'zero? '(rational) rational-zero?)
  (put 'raise '(rational) raise-rational)
  (put 'project '(rational) project-rational)
  'done)

;; 构造函数
(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)
