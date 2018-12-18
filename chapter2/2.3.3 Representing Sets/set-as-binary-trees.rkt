;; Exercise 2.63~2.66
;; 二叉树构成的集合
#lang racket

;; 树定义
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; 判断元素是否存在于集合
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;; 向集合中插入元素
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; 前序遍历二叉树
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;; 逆前序遍历二叉树
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; 有序列表转换为平衡树
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons null elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (entry (cadr left-result))
                (right-elts (cddr left-result))
                (right-size (- n left-size 1)))
            (let ((right-result (partial-tree right-elts right-size)))
              (let ((right-tree (car right-result))
                    (remains (cdr right-result)))
                (cons (make-tree entry left-tree right-tree)
                      remains))))))))

;; 取交集
(define (union-set set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    ;; 取两个有序列表交集
    (define (merge list1 list2)
      (cond ((null? list1) list2)
            ((null? list2) list1)
            (else
             (let ((x1 (car list1))
                   (x2 (car list2)))
               (cond ((= x1 x2) (cons x1 (merge (cdr list1) (cdr list2))))
                     ((< x1 x2) (cons x1 (merge (cdr list1) list2)))
                     (else (cons x2 (merge list1 (cdr list2)))))))))
    (let ((union-list (merge list1 list2)))
      (list->tree union-list))))

;; 取交集
(define (intersection-set set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-2 set2)))
    ;; 取两个有序列表并集
    (define (merge list1 list2)
      (if (or (null? list1) (null? list2))
          null
          (let ((x1 (car list1))
                (x2 (car list2)))
            (cond ((= x1 x2) (cons x1 (merge (cdr list1) (cdr list2))))
                  ((< x1 x2) (merge (cdr list1) list2))
                  (else (merge list1 (cdr list2)))))))
    (let ((intersection-list (merge list1 list2)))
      (list->tree intersection-list))))

;; 查找
(define (lookup set1 key)
  (if (null? set1)
      false
      (let ((x (entry set1)))
        (cond ((= x key) true)
              ((< x key) (lookup (right-branch set1) key))
              (else (lookup (left-branch set1) key))))))


  

