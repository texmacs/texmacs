
;; arch-tag: e85790a7-25de-4c7a-b6db-eea9d5e4c3c5

(texmacs-module (texmacs edit smart-space)
  (:use (utils library tree)))

(define (string-space-at? s i) (== #\space (string-ref s i)))
(define (tree-func? t s) (== s (tree-label t)))

; (define (near-space?/leaf t1 i)
;   (let ((s (tree->stree t1)))
;     (or (and (< 0 i)
;              (string-space-at? s (1- i)))
;         (and (< i (string-length s))
;              (string-space-at? s i)))))

; (define (near-space?/after p1)
;   (and-let* ((t2 (path->tree (cDr p1)))
;              ((tree-func? t2 'concat))
;              (i (cAr p1))
;              ((< (1+ i) (tree-arity t2)))
;              (next-t (tree-ref t2 (1+ i)))
;              ((tree-atomic? next-t))
;              ((string-space-at? (tree->string next-t) 0)))))

; (define (near-space?)
;   (let* ((p (cursor-path))
;          (p1 (cDr p))
;          (i (cAr p))
;          (t1 (path->tree p1)))
;     (cond ((tree-atomic? t1) (near-space?/leaf t1 i))
;           ((== i 1) (near-space?/after p1))
;           (else #f))))

; (define (smart-space)
;     (if (not (near-space?)) (insert " ")))

;(set-trace-level! near-space? near-space?/leaf near-space?/after)

(tm-define (smart-space)
  (cond ((at-start?) (noop))
	((after-space?) (noop))
	((before-space?) (move-next))
	(else (insert " "))))

(define (at-start?)
  (let ((p (but-last (cursor-path)))
	(i (last (cursor-path))))
    (let ((t (path->tree p)))
      (and (tree-atomic? t) (zero? i)))))

(define (before-space?)
  (let ((p (but-last (cursor-path)))
	(i (last (cursor-path))))
    (let ((t (path->tree p)))
      (cond ((tree-atomic? t) (before-space?/atomic t i))
	    (else #f)))))

(define (before-space?/atomic t i)
  (let ((s (tree->stree t)))
    (and (< i (string-length s))
	 (string-space-at? s i))))

(define (after-space?)
  (let ((p (but-last (cursor-path)))
	(i (last (cursor-path))))
    (let ((t (path->tree p)))
      (cond ((tree-atomic? t) (after-space?/atomic t i))
	    (else #f)))))

(define (after-space?/atomic t i)
  (let ((s (tree->stree t)))
    (and (< 0 i) (string-space-at? s (1- i)))))

(define (move-next)
  (let ((p (but-last (cursor-path)))
	(i (last (cursor-path))))
    (let ((t (path->tree p)))
      (if (tree-atomic? t)
	  (let ((s (tree->stree t)))
	    (if (< i (string-length s))
		(go-to (rcons p (1+ i)))))))))
