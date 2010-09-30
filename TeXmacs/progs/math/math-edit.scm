
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-edit.scm
;; DESCRIPTION : editing mathematics
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-edit)
  (:use (utils library tree)
	(utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some drd properties, which should go into table-drd.scm later on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group variant-tag (math-table-tag))
(define-group similar-tag (math-table-tag))

(define-group math-table-tag
  matrix det)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special customizations inside equation environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-return)
  (:inside equation)
  (go-end-of 'equation)
  (insert-return))

(tm-define (kbd-return)
  (:inside equation*)
  (go-end-of 'equation*)
  (insert-return))

(tm-define (make-label)
  (:inside eqnarray*)
  (go-end-line)
  (make 'label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for moving punctuation symbols around
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-ref-nspace? s i)
  (!= (string-ref s i) #\space))

(define (string-ref-npunct? s i)
  (nin? (string-ref s i) '(#\space #\. #\, #\: #\;)))

(define (string-search-forwards s i n pred?)
  (cond ((>= i n) i)
	((pred? s i) i)
	(else (string-search-forwards s (+ i 1) n pred?))))

(define (string-search-backwards s i b pred?)
  (cond ((<= i b) i)
	((pred? s (- i 1)) i)
	(else (string-search-backwards s (- i 1) b pred?))))

(define (atomic-cut-left-until t pred?)
  (if (atomic-tree? t)
      (let* ((s (tree->string t))
	     (n (string-length s))
	     (i (string-search-forwards s 0 n pred?)))
	(if (> i 0)
	    (with ss (substring s 0 i)
	      (tree-remove! t 0 i)
	      (tree-correct t)
	      ss)
	    ""))
      ""))

(define (atomic-cut-right-until t pred?)
  (if (atomic-tree? t)
      (let* ((s (tree->string t))
	     (n (string-length s))
	     (i (string-search-backwards s n 0 pred?)))
	(if (< i n)
	    (with ss (substring s i n)
	      (tree-remove! t i (- n i))
	      (tree-correct t)
	      ss)
	    ""))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switching between different types of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (concat-isolate! t)
  `(cond ((not (tree-is? t :up 'concat)) (noop))
	 ((not (tree-is? t :up :up 'document)) (noop))
	 ((= (tree-arity (tree-up t)) 1) (tree-set! t :up t))
	 ((< (tree-index t) (- (tree-arity (tree-up t)) 1))
	  (tree-split (tree-up t 2)
		      (tree-index (tree-up t))
		      (+ (tree-index t) 1))
	  (concat-isolate! t))
	 (else
	  (tree-split (tree-up t 2)
		      (tree-index (tree-up t))
		      (tree-index t))
	  (concat-isolate! t))))

(define (math->equation*)
  (with-innermost t 'math
    (let* ((c (and (tree-is? t :up 'concat) (tree-is? t :up :up 'document)))
	   (r (and c (atomic-cut-left-until (tree-ref t :next)
					    string-ref-npunct?)))
	   (l (and c (atomic-cut-right-until (tree-ref t :previous)
					     string-ref-nspace?))))
      (concat-isolate! t)
      (if (tree-is? t :up 'document)
	  (begin
	    (if (not r) (set! r ""))
	    (if (not l) (set! l ""))
	    (tree-set! t `(equation* (document ,(tree-ref t 0))))
	    (while (string-ends? r " ")
	      (set! r (string-drop-right r 1)))
	    (with-cursor (tree->path t 0 0 :end)
	      (insert r)))))))

(define (equation*->math)
  (with-innermost t '(equation equation*)
    (if (or (not (tree-is? t 0 'document))
	    (= (tree-arity (tree-ref t 0)) 1))
	(begin
	  (if (tree-is? t 0 'document)
	      (tree-set! t 0 (tree-ref t 0 0)))
	  (tree-set! t `(math ,(tree-ref t 0)))
	  (with r (atomic-cut-right-until (tree-end (tree-ref t 0))
					  string-ref-npunct?)
	    (with-cursor (tree->path t :start)
	      (kbd-remove #f)
	      (if (and (!= (cursor-path) (cursor-after (go-start-paragraph)))
		       (!= (cursor-path) (cursor-after (go-end-paragraph))))
		  (insert " ")))
	    (with-cursor (tree->path t :end)
	      (insert r)
	      (kbd-remove #t)
	      (if (and (!= (cursor-path) (cursor-after (go-start-paragraph)))
		       (!= (cursor-path) (cursor-after (go-end-paragraph))))
		  (insert " "))))))))

(define (with-math-context? t)
  (match? t '(with "mode" "math" :%1)))

(tm-define (variant-circulate forward?)
  (:context with-math-context?)
  (with-innermost t with-math-context?
    (tree-set! t `(math ,(tree-ref t 2)))
    (math->equation*)))

(tm-define (variant-circulate forward?)
  (:inside math)
  (math->equation*))

(tm-define (variant-circulate forward?)
  (:inside equation equation*)
  (equation*->math))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Management of groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (group-set-left t l)
  (cond ((tree-func? t 'group 1)
	 (tree-assign-node t 'rigid) ;; temporarily force editable border
	 (group-set-left (tree-ref t 0) l)
	 (tree-assign-node t 'group))
	((and (tree-func? t 'concat) (> (tree-arity t) 0))
	 (group-set-left (tree-ref t 0) l))
	((or (tree-func? t 'left)
	     (tree-func? t 'big)
	     (and (tree-atomic? t) (!= (tree->string t) "")))
	 (with-cursor (tree->path t :start)
	   (remove-text #t)
	   (insert l)))
	(else (texmacs-error "group-set-left"
			     "cannot set left parenthesis in ~S" t))))

(tm-define (group-set-right t r)
  (cond ((tree-func? t 'group 1)
	 (tree-assign-node t 'rigid) ;; temporarily force editable border
	 (group-set-right (tree-ref t 0) r)
	 (tree-assign-node t 'group))
	((and (tree-func? t 'concat) (> (tree-arity t) 0))
	 (group-set-right (tree-ref t :last) r))
	((or (tree-func? t 'right)
	     (tree-func? t 'big)
	     (and (tree-atomic? t) (!= (tree->string t) "")))
	 (with-cursor (tree->path t :end)
	   (remove-text #f)
	   (insert r)))
	(else (texmacs-error "group-set-right"
			     "cannot set right parenthesis in ~S" t))))

(tm-define (group-get-left t)
  (cond ((tree-func? t 'group 1)
	 (group-get-left (tree-ref t 0)))
	((and (tree-func? t 'concat) (> (tree-arity t) 0))
	 (group-get-left (tree-ref t 0)))
	((or (tree-func? t 'left) (tree-func? t 'big)) t)
	((and (tree-atomic? t) (!= (tree->string t) ""))
	 (tmstring-ref (tree->string t) 0))
	(else #f)))

(tm-define (group-get-right t)
  (cond ((tree-func? t 'group 1)
	 (group-get-left (tree-ref t 0)))
	((and (tree-func? t 'concat) (> (tree-arity t) 0))
	 (group-get-left (tree-ref t :last)))
	((or (tree-func? t 'right) (tree-func? t 'big)) t)
	((and (tree-atomic? t) (!= (tree->string t) ""))
	 (tmstring-reverse-ref (tree->string t) 0))
	(else #f)))

(tm-define (group-empty-left? t)
  (in? (group-get-left t)
       (map tm->tree '((left ".") (big ".") "<lnone>"))))

(tm-define (group-empty-right? t)
  (in? (group-get-right t)
       (map tm->tree '((right ".") (big ".") "<rnone>"))))

(tm-define (group-set-empty-left t)
  (if (and (tree-func? t 'group 1) (group-empty-right? t))
      (with-cursor (tree->path t 0 :start)
	(remove-structure-upwards))
      (and-with l (group-get-left t)
	(cond ((tree-func? l 'left) (group-set-left '(left ".")))
	      ((tree-func? l 'big) (group-set-left '(big ".")))
	      ((tree-atomic? l) (group-set-left "<lnone>"))))))

(tm-define (group-set-empty-right t)
  (if (and (tree-func? t 'group 1) (group-empty-left? t))
      (with-cursor (tree->path t 0 :start)
	(remove-structure-upwards))
      (and-with r (group-get-right t)
	(cond ((tree-func? r 'right) (group-set-left '(right ".")))
	      ((tree-func? r 'big) (group-set-left '(big ".")))
	      ((tree-atomic? r) (group-set-left "<rnone>"))))))
