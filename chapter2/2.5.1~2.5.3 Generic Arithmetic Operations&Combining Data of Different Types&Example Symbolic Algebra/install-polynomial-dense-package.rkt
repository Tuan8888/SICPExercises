;; 多项式包，稠密表示法
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "apply-generic.rkt")
(require "install-polynomial-parse-package.rkt")
(provide (all-defined-out))

(define (install-polynomial-dense-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; 返回稀疏表示法的稀疏list
  (define (parse-terms p)
    (define (iter cur remains res)
      (if (null? remains)
          res
          (iter (+ cur 1) (cdr remains) (cons (list cur (car remains)) res))))
    (iter 0 p null))
  ;; 稠密转稀疏
  (define (dense-to-parse p)
    (let ((content (contents p)))
      (make-polynomial-parse (variable content) (parse-terms (term-list content)))))
  ;; 判断是否是同一变量
  (define (same-variable? x y)
    (equal? x y))
  ;; 多项式加法
  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  ;; 多项式乘法
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  ;; 多项式减法
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ;; term-list
  (define (sub-terms L1 L2)
    ;; 将多项式的一项系数取负
    (define (negation-term term)
      (apply-generic 'sub 0 term))
    (add-terms L1 (map negation-term L2)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (cons (apply-generic 'add (first-term L1) (first-term L2))
                 (add-terms (rest-terms L1) (rest-terms L2))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) (cons 0 L2)))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (map (lambda (x) (apply-generic 'mul x t1)) L)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial-dense p))
  (put 'add '(polynomial-dense polynomial-dense) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial-dense polynomial-dense) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial-dense polynomial-dense) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial-dense
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'coercion '(polynomial-dense polynomial-parse) dense-to-parse)
  'done)

;; 构造函数
(define (make-polynomial-dense variable term-list)
  ((get 'make 'polynomial-dense) variable term-list))

(install-polynomial-dense-package)
