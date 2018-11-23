;; Exercise 2.33~2.39
;; sequence相关操作
#lang racket
(provide (all-defined-out))

;; 过滤
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

;; 累计
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 枚举树
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((pair? tree) (append (enumerate-tree (car tree))
                              (enumerate-tree (cdr tree))))
        (else (list tree))))

;; map-by-accumulate
(define (map-by-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

;; append-by-accumulate
(define (append-by-accumulate seq1 seq2)
  (accumulate cons seq2 seq1))

;; length-by-accumulate
(define (length-by-accumulate sequence)
  (accumulate (lambda(x y) (+ y 1)) 0 sequence))

;; polynomial-by-accumulate
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+
                                                  (* higher-terms x)
                                                  this-coeff))
               0
               coefficient-sequence))

;; count-leaves-by-accumulate
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda(x)
                     (cond ((null? x) 0)
                           ((pair? x) (count-leaves x))
                           (else 1)))
                   t)))
                    
;; 若干个list的对应位累计
(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op initial (map (lambda(x)
                                          (car x))
                                        seqs))
            (accumulate-n op initial (map (lambda(x)
                                            (cdr x))
                                          seqs)))))

;; 向量点乘
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; 矩阵向量乘
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

;; 矩阵转置
(define (transpose mat)
  (accumulate-n cons null mat))

;; 矩阵乘矩阵
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(x) (matrix-*-vector cols x)) m)))

;; 向左fold
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; 向右fold
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;; 翻转
(define (reverse sequence)
  (fold-right (lambda(x y)
                (append y (list x)))
              null
              sequence))

