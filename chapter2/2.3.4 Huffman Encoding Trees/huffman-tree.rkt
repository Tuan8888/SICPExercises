;; Exercise 2.67~2.70
;; 霍夫曼树
#lang racket

;; 叶节点定义
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; 编码树节点定义
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 比特流解码
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; 根据当前比特决定选择左分支还是右分支
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; 节点保存的集合，输入为一组symbol和weight，集合将按照weight大小排序
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;; 向有序集合中插入数据，已确保插入数据无重复
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; 解码测试
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;; 编码
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; 编码一个symbol
(define (encode-symbol symbol tree)
  ;; 判断有序集合中是否含有某元素
  (define (contains? set1 s)
    (cond ((null? set1) false)
          ((equal? (car set1) s) true)
          (else (contains? (cdr set1) s))))
  ;; 判断子树是否包含symbol
  (define (tree-contains? s t)
    (contains? (symbols t) s))
  (cond ((leaf? tree) null)
        ((tree-contains? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((tree-contains? symbol (right-branch tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error ("待加密信息输入错误")))))
         
;; 测试编码      
(define sample-characters '(A D A B B C A))
(encode sample-characters sample-tree)

;; 生成霍夫曼树
;; 输入为一组symbol-weight对
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; 将最小的元素合并为一个节点
(define (successive-merge ordered-pairs)
  (if (= (length ordered-pairs) 1)
      (car ordered-pairs)
      (let ((leaf1 (car ordered-pairs))
            (leaf2 (cadr ordered-pairs))
            (remains (cddr ordered-pairs)))
        (successive-merge (adjoin-set (make-code-tree leaf1 leaf2) remains)))))

(define lyric-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM  1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(+ (* (length (encode '(GET A JOB) lyric-tree)) 2)
   (* (length (encode '(SHA NA NA NA NA NA NA NA NA) lyric-tree)) 2)
   (length (encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) lyric-tree))
   (length (encode '(SHA BOOM) lyric-tree)))

