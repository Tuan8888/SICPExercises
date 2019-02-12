;; 多项式包，稀疏表示法
#lang racket
(require (file "../2.4.3 Data-Directed Programming and Additivity/funcs-table.rkt"))
(require "tag-datum.rkt")
(require "variable-priority.rkt")
(require "apply-generic.rkt")
(provide (all-defined-out))

(define (install-polynomial-parse-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; 判断是否是同一变量
  (define (same-variable? x y)
    (equal? x y))
  ;; 多项式加法
  (define (add-poly p1 p2)
    (let ((var1 (variable p1))
          (var2 (variable p2)))
      (if (same-variable? var1 var2)
          (make-poly var1
                     (add-terms (term-list p1)
                                (term-list p2)))
          ;; 如果变量不同，判断优先级
          (if (higher-priority? var1 var2)
              (add-poly p1 (make-poly var1 (list (list 0 p2))))
              (add-poly (make-poly var2 (list (list 0 p1))) p2)))))
  ;; 多项式除法
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((res (div-terms (term-list p1) (term-list p2))))
          (list (tag (make-poly (variable p1)
                                (car res)))
                (tag (make-poly (variable p2)
                                (cadr res)))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  ;; 多项式乘法
  (define (mul-poly p1 p2)
    (let ((var1 (variable p1))
          (var2 (variable p2)))
      (if (same-variable? var1 var2)
          (make-poly (variable p1)
                     (mul-terms (term-list p1)
                                (term-list p2)))
          (if (higher-priority? var1 var2)
              (mul-poly p1 (make-poly var1 (list (list 0 p2))))
              (mul-poly (make-poly var2 (list (list 0 p1))) p2)))))
  ;; 多项式减法
  (define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))
  ;; 两个多项式化简
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((res (reduce-terms (term-list p1) (term-list p2))))
          (list (tag (make-poly (variable p1)
                                (car res)))
                (tag (make-poly (variable p2)
                                (cadr res)))))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))
  ;; 求公因子
  (define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))
  ;; term-list
  (define (sub-terms L1 L2)
    ;; 将多项式的一项系数取负
    (define (negation-term term)
      (list (order term) (apply-generic 'negation (coeff term))))
    (add-terms L1 (map negation-term L2)))
  (define (add-terms L1 L2)
     (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  ;; 加法计算
  ;; 如果两项均为数字，直接调用该数字类型的加法
  ;; 如果有多项式，则数字升级为多项式，进行加法计算
  ;; 如果是两个多项式，直接调用add-poly
  (define (add n1 n2)
    (cond ((and (isNumber? n1) (isNumber? n2)) (apply-generic 'add n1 n2))
          ((isNumber? n1) (tag (add-poly (contents (apply-generic 'to-polynomial (variable n2) n1)) n2)))
          ((isNumber? n2) (tag (add-poly n1 (contents (apply-generic 'to-polynomial (variable n1) n2)))))
          (else (add-poly n1 n2))))
  ;; 判断是否是数字
  (define (isNumber? x)
    (let ((flag (get 'isNumber? (type-tag x))))
      (not (null? flag))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ;; 乘法计算
  ;; 如果两个都是数字，调用apply-generic
  ;; 如果是多项式和数字，把数字变为多项式计算
  ;; 如果是两个多项式，直接调用mul-poly
  (define (mul n1 n2)
    (cond ((and (isNumber? n1) (isNumber? n2)) (apply-generic 'mul n1 n2))
          ((isNumber? n1) (tag (mul-poly (contents (apply-generic 'to-polynomial (variable n2) n1)) n2)))
          ((isNumber? n2) (tag (mul-poly n1 (contents (apply-generic 'to-polynomial (variable n1) n2)))))
          (else (mul-poly n1 n2))))
  ;; 除法
  (define (div-terms L1 L2)
      (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (apply-generic 'div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms
                        (sub-terms
                         L1
                         (mul-terms L2 (list (list new-o new-c))))
                        L2)
                       ))
                  (list (adjoin-term
                         (list new-o new-c)
                         (car rest-of-result))
                        (cadr rest-of-result))
                  ))))))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  ;; 判断该项是否为0
  (define (=zero? n1)
    (if (isNumber? n1)
        (apply-generic 'zero? n1)
        (null? (term-list n1))))
  ;; 系数化简
  (define (simplify-terms a)
    (if (empty-termlist? a)
        a
        (let ((coeffs (map coeff a)))
          (let ((g ((get 'gcd (type-tag (car coeffs))) coeffs)))
            (map (lambda (t) (make-term (order t) (apply-generic 'div (coeff t) g))) a)))))
  (define (gcd-terms a b)
    (simplify-terms (gcd-terms-iter a b)))
  (define (gcd-terms-iter a b)
    ;; 求余
    (define (remainder-terms a b)
      (cadr (div-terms a b)))
    ;; pseudoremainder-terms，保证结果不出现分数
    (define (pseudoremainder-terms a b)
      (let ((t1 (first-term a))
            (t2 (first-term b)))
          (let ((c (coeff t2))
                (o1 (order t1))
                (o2 (order t2)))
            (let ((factor (apply-generic 'pseudo-factor c o1 o2)))
              (cadr (div-terms (map
                                (lambda (x)
                                  (make-term (order x)
                                             (apply-generic 'mul (coeff x) factor)))
                                a)
                               b))))))
    (if (empty-termlist? b)
        a
        (gcd-terms-iter b (pseudoremainder-terms a b))))
  (define (reduce-terms a b)
    ;; 系数列表中每个系数乘以或除以一个因子
    (define (factor-terms a factor op)
      (map (lambda (t) (make-term (order t) (apply-generic op (coeff t) factor))) a))
    (let ((g (gcd-terms a b)))
      (let ((factor1 (apply-generic 'pseudo-factor (coeff (first-term g)) (order (first-term a)) (order (first-term g))))
            (factor2 (apply-generic 'pseudo-factor (coeff (first-term g)) (order (first-term b)) (order (first-term g)))))
        (list
         (factor-terms (car (div-terms (factor-terms a factor1 'mul) g)) factor1 'div)
         (factor-terms (car (div-terms (factor-terms b factor2 'mul) g)) factor2 'div)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial-parse p))
  (put 'add '(polynomial-parse polynomial-parse) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial-parse polynomial-parse) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial-parse polynomial-parse) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial-parse polynomial-parse) 
       (lambda (p1 p2) (div-poly p1 p2)))
  (put 'zero? '(polynomial-parse)
       (lambda (p) (null? (term-list p))))
  (put 'greatest-common-divisor
       '(polynomial-parse polynomial-parse)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'polynomial-parse
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'reduce '(polynomial-parse polynomial-parse)
       (lambda (p1 p2) (reduce-poly p1 p2)))
  'done)

;; 构造函数
(define (make-polynomial-parse variable term-list)
  ((get 'make 'polynomial-parse) variable term-list))

(install-polynomial-parse-package)
