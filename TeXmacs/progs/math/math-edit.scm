
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
	(utils library cursor)
	(utils edit auto-close)))

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

(tm-define (math-make-math)
  (if (inside? 'math)
      (go-end-of 'math)
      (set-message (string-append "Warning: already inside mathematics")
		   (string-append "make 'math'"))))

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
;; Modifying the size and shape of brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-small br)
  (cond ((tree? br) (make-small (tree->stree br)))
	((string? br) br)
	((not (or (tm-func? br 'left) (tm-func? br 'right))) "<nobracket>")
	(else
	  (with s (tm-ref br 0)
	    (cond ((nstring? s) "<nobracket>")
		  ((== s ".") "<nobracket>")
		  ((== (string-length s) 1) s)
		  (else (string-append "<" s ">")))))))

(define (make-large s pos)
  (with type (if (== pos 0) 'left 'right)
    (cond ((tree? s) (make-large (tree->stree s) pos))
	  ((or (tm-func? s 'left) (tm-func? s 'right)) s)
	  ((nstring? s) `(,type "."))
	  ((== s "<nobracket>") `(,type "."))
	  ((== (string-length s) 1) `(,type ,s))
	  ((and (string-starts? s "<") (string-ends? s ">"))
	   `(,type ,(substring s 1 (- (string-length s) 1))))
	  (else `(,type ".")))))

(tm-define (toggle-variant)
  (:inside around)
  (with-innermost t 'around
    (tree-assign-node t 'around*)))

(tm-define (toggle-variant)
  (:inside around*)
  (with-innermost t 'around*
    (tree-assign-node t 'around)))

(define brackets
  '(("(" ")")
    ("[" "]")
    ("{" "}")
    ("<langle>" "<rangle>")
    ("|" "|")
    ("<||>" "<||>")
    ("<lfloor>" "<rfloor>")
    ("<lceil>" "<rceil>")
    ("<llbracket>" "<rrbracket>")))

(tm-define (variant-circulate forward?)
  (:inside around around*)
  (with-innermost t '(around around*)
    (when (and (== (tree-arity t) 3)
	       (tree-atomic? (tree-ref t 0))
	       (tree-atomic? (tree-ref t 2)))
      (let* ((l (tree->string (tree-ref t 0)))
	     (r (tree->string (tree-ref t 2)))
	     (p (list l r)))
	(when (in? p brackets)
	  (let* ((i (list-find-index brackets (lambda (x) (== x p))))
		 (j (modulo (+ i (if forward? 1 -1)) (length brackets)))
		 (nl (car (list-ref brackets j)))
		 (nr (cadr (list-ref brackets j))))
	    (tree-assign (tree-ref t 0) nl)
	    (tree-assign (tree-ref t 2) nr)))))))

(define bigops
  '("<int>" "<intlim>" "<oint>" "<ointlim>"
    "<sum>" "<prod>" "<amalg>"
    "<cap>" "<cup>" "<sqcap>" "<sqcup>"
    "<vee>" "<wedge>" "<curlyvee>" "<curlywedge>"
    "<odot>" "<otimes>" "<oplus>"
    "<triangleup>" "<triangledown>"
    "<box>" "<parallel>" "<interleave>"))

(tm-define (variant-circulate forward?)
  (:inside big-around)
  (with-innermost t 'big-around
    (when (and (== (tree-arity t) 2)
	       (tree-atomic? (tree-ref t 0)))
      (with s (tree->string (tree-ref t 0))
	(when (in? s bigops)
	  (let* ((i (list-find-index bigops (lambda (x) (== x s))))
		 (j (modulo (+ i (if forward? 1 -1)) (length bigops)))
		 (ns (list-ref bigops j)))
	    (tree-assign (tree-ref t 0) ns)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deleted? t i)
  (== (tm->stree (tree-ref t i)) "<nobracket>"))

(define (make-small s)
  (cond ((nstring? s) "<nobracket>")
	((== s ".") "<nobracket>")
	((<= (string-length s) 1) s)
	(else (string-append "<" s ">"))))

(define (find-adjacent-around del?)
  (let* ((ret #f)
	 (p (cursor-path))
	 (p* (cursor-path*)))
    (with t (tree-innermost '(around around*))
      (when t
	(when (== p (tree->path t 1 :start))
	  (when (deleted? t 0)
	    (set! ret t)))
	(when (== p (tree->path t 1 :end))
	  (when (or (not del?) (deleted? t 2))
	    (set! ret t)))))
    (when (not ret)
      (with t (path->tree (cDr p))
	(when (tree-in? t '(around around*))
	  (when (== (cAr p) 0)
	    (when (deleted? t 0)
	      (set! ret t)))
	  (when (== (cAr p) 1)
	    (when (or (not del?) (deleted? t 2))
	      (set! ret t))))))
    (when (and (not ret) (!= p p*))
      (with t (path->tree (cDr p*))
	(when (tree-in? t '(around around*))
	  (when (== (cAr p*) 0)
	    (when (deleted? t 0)
	      (set! ret t))))))
    ret))

(tm-define (math-bracket-open lb rb large?)
  (when (!= (get-preference "matching brackets") "on")
    (make-bracket-open lb rb large?))
  (when (== (get-preference "matching brackets") "on")
    (if large? (set! lb (make-small lb)))
    (if large? (set! rb (make-small rb)))
    (let* ((t (find-adjacent-around #t))
	   (u (find-adjacent-around #f)))
      (cond ((and t (deleted? t 0))
	     (tree-set t 0 lb)
	     (tree-go-to t 1 :start))
	    ((and t (deleted? t 2))
	     (tree-set t 2 lb)
	     (tree-go-to t :end))
	    ((and u (== lb rb) (== (tree->stree (tree-ref u 2)) rb))
	     (tree-go-to u :end))
	    ((and u (== (tree->stree (tree-ref u 0)) "<langle>") (== rb "|"))
	     (tree-set u 2 rb)
	     (tree-go-to u :end))
	    ((not large?)
	     (insert-go-to `(around ,lb "" ,rb) '(1 0)))
	    (large?
	     (insert-go-to `(around* ,lb "" ,rb) '(1 0)))))))

(tm-define (math-separator sep large?)
  (when (!= (get-preference "matching brackets") "on")
    (make-separator sep large?))
  (when (== (get-preference "matching brackets") "on")
    (make-separator sep large?)))

(tm-define (math-bracket-close rb lb large?)
  (when (!= (get-preference "matching brackets") "on")
    (make-bracket-close rb lb large?))
  (when (== (get-preference "matching brackets") "on")
    (if large? (set! rb (make-small rb)))
    (if large? (set! lb (make-small lb)))
    (let* ((t (find-adjacent-around #t))
	   (u (find-adjacent-around #f)))
      (cond ((and t (deleted? t 0))
	     (tree-set t 0 rb)
	     (tree-go-to t 1 :start))
	    ((and t (deleted? t 2))
	     (tree-set t 2 rb)
	     (tree-go-to t :end))
	    (u
	     (tree-set u 2 rb)
	     (tree-go-to u :end))
	    (else
	      (set-message "Error: bracket does not match"
			   (force-string rb)))))))

(tm-define (math-big-operator op)
  (when (!= (get-preference "matching brackets") "on")
    (make-big-operator op))
  (when (== (get-preference "matching brackets") "on")
    (insert-go-to `(big-around ,(make-small op) "") '(1 0))))
