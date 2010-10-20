
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-edit.scm
;; DESCRIPTION : routines for formatting text
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-edit)
  (:use (utils base environment)
	(utils edit selections)
	(generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-env? var val)
  (== (get-env var) val))

(tm-property (make-with var val)
  (:check-mark "o" test-env?))

(tm-define (make-interactive-with var)
  (:interactive #t)
  (interactive (lambda (s) (make-with var s))
    (list (drd-ref env-var-description% var) "string" (get-env var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting and toggling with-like tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-like-search t)
  (if (with-like? t) t
      (and (or (tree-atomic? t) (tree-in? t '(concat document)))
	   (and-with p (tree-ref t :up)
	     (with-like-search p)))))

(tm-define (with-like-check-insert t)
  (cond ((with u (cursor-tree)
	   (and (with-like? u) (with-same-type? t u)))
	 (with u (cursor-tree)
	   (tree-go-to u :last (if (== (cAr (cursor-path)) 0) :start :end))
	   #t))
	((with u (cursor-tree*)
	   (and (with-like? u) (with-same-type? t u)))
	 (with u (cursor-tree*)
	   (tree-go-to u :last :start)
	   #t))
	((and-with u (with-like-search (cursor-tree)) (with-same-type? t u))
	 (with sym (symbol->string (tree-label t))
	   (set-message `(concat "Warning: already inside '" ,sym "'")
			`(concat "make '" ,sym "'"))
	   #t))
	(else #f)))

(tm-define (make-with-like w)
  (cond ((func? w 'with 3)
	 (make-with (cadr w) (caddr w)))
	((and (tm-compound? w) (== (tm-arity w) 1))
	 (make (car w)))
	((selection-active-any?)
	 (let* ((selection (selection-tree))
		(ins `(,@(cDr w) ,selection))
		(end (path-end ins '())))
	   (clipboard-cut "nowhere")
	   (insert-go-to ins (cons (- (tm-arity ins) 1) end))))
	(else
	  (insert-go-to w (list (- (tm-arity w) 1) 0)))))

(tm-define (toggle-with-like w)
  (with t (if (and (selection-active-any?)
		   (== (selection-tree) (path->tree (selection-path))))
	      (path->tree (selection-path))
	      (with-like-search (tree-ref (cursor-tree) :up)))
    ;;(display* "t= " t "\n")
    (if (and t (with-like? t) (with-same-type? t w))
	(begin
	  (tree-remove-node! t (- (tree-arity t) 1))
	  (tree-correct-node (tree-ref t :up)))
	(make-with-like w))))

(tm-define (toggle-bold)
  (toggle-with-like '(with "font-series" "bold" "")))

(tm-define (toggle-italic)
  (toggle-with-like '(with "font-shape" "italic" "")))

(tm-define (toggle-small-caps)
  (toggle-with-like '(with "font-shape" "small-caps" "")))

(tm-define (toggle-underlined)
  (toggle-with-like '(underline "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (make-var-space spc base top)
  (:argument spc "Horizontal space")
  (:argument base "Base level")
  (:argument top "Top level"))

(tm-property (make-hspace spc)
  (:argument spc "Horizontal space"))

(tm-property (make-space spc)
  (:argument spc "Horizontal space"))

(tm-property (make-htab spc)
  (:argument spc "Minimal space"))

(tm-property (make-vspace-before spc)
  (:argument spc "Vertical space"))

(tm-property (make-vspace-after)
  (:argument spc "Vertical space"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-page-break)
  (make 'page-break)
  (insert-return))

(tm-define (make-new-page)
  (make 'new-page)
  (insert-return))

(tm-define (make-new-dpage)
  (make 'new-dpage)
  (insert-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resizing spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (space-context? t)
  (and-with u (tree-down t)
    (and (tm-func? u 'space)
	 (tm-length? (tree-ref u 0)))))

(tm-define (vspace-context? t)
  (and-with u (tree-down t)
    (and (or (tm-func? u 'vspace 1) (tm-func? u 'vspace* 1))
	 (tm-length? (tree-ref u 0)))))

(define (add-space t factor)
  (when (tm-length? t)
    (let* ((l (tree->string t))
	   (v (tm-length-value l))
	   (u (tm-length-unit l))
	   (a (if (== u "spc") 0.2 1))
	   (new-v (+ v (* factor a)))
	   (new-l (tm-make-length new-v u)))
      (tree-set t new-l))))

(define (space-make-ternary t)
  (cond ((== (tm-arity t) 1) (tree-insert t 1 '("0ex" "1ex")))
	((== (tm-arity t) 2) (tree-insert t 1 '("1ex")))))

(tm-define (geometry-left)
  (:context space-context?)
  (with-innermost t space-context?
    (add-space (tree-ref t :down 0) -1)))

(tm-define (geometry-right)
  (:context space-context?)
  (with-innermost t space-context?
    (add-space (tree-ref t :down 0) 1)))

(tm-define (geometry-up)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (add-space (tree-ref t 2) 1))))

(tm-define (geometry-down)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (add-space (tree-ref t 2) -1))))

(tm-define (geometry-top)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (add-space (tree-ref t 1) 1))))

(tm-define (geometry-bottom)
  (:context space-context?)
  (with-innermost p space-context?
    (with t (tree-ref p :down)
      (space-make-ternary t)
      (add-space (tree-ref t 1) -1))))

(tm-define (geometry-up)
  (:context vspace-context?)
  (with-innermost t vspace-context?
    (add-space (tree-ref t :down 0) -1)))

(tm-define (geometry-down)
  (:context vspace-context?)
  (with-innermost t vspace-context?
    (add-space (tree-ref t :down 0) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-move hor ver)
  (:argument hor "Horizontal")
  (:argument ver "Vertical")
  (wrap-selection-small
    (insert-go-to `(move "" ,hor ,ver) '(0 0))))

(tm-define (make-shift hor ver)
  (:argument hor "Horizontal")
  (:argument ver "Vertical")
  (wrap-selection-small
    (insert-go-to `(shift "" ,hor ,ver) '(0 0))))

(tm-define (make-resize l b r t)
  (:argument l "Left")
  (:argument b "Bottom")
  (:argument r "Right")
  (:argument t "Top")
  (wrap-selection-small
    (insert-go-to `(resize "" ,l ,b ,r ,t) '(0 0))))

(tm-define (make-clipped l b r t)
  (:argument l "Left")
  (:argument b "Bottom")
  (:argument r "Right")
  (:argument t "Top")
  (wrap-selection-small
    (insert-go-to `(clipped "" ,l ,b ,r ,t) '(0 0))))
