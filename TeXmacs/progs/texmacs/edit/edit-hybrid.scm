
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : hybrid.scm
;; DESCRIPTION : routines which strongly depend on the context
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-hybrid)
  (:use
    (texmacs edit edit-text) (texmacs edit edit-table)
    (texmacs edit edit-session) (texmacs edit edit-fold)
    (texmacs edit edit-title))
  (:export
    make-return make-shift-return
    structured-insert-left structured-insert-right
    structured-insert-up structured-insert-down
    structured-remove
    position-default position-left position-right position-up position-down
    position-start position-end position-top position-bottom
    general-remove general-remove general-tab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The multi-purpose return key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (return-inside-table)
  (let ((x (inside-which '("table" "document"))))
    (if (== x "document")
	(insert-return)
	(begin
	  (table-insert-row #t)
	  (table-go-to (table-which-row) 1)))))

(define (return-sectional)
  (go-end-line)
  (insert-return))

(define (make-return-inside x)
  (cond ((== x "hybrid") (activate-hybrid #f))
	((== x "latex") (activate-latex))
	((== x "symbol") (activate-symbol))
	((== x "inactive") (activate))
	((== x "title")
	 (if (inside? "make-title")
	     (make-header 'author)
	     (return-sectional)))
	((== x "author")
	 (if (inside? "make-title")
	     (make-header 'address)
	     (return-sectional)))
	((== x "doc-title") (make-doc-data-element 'doc-author-data))
	((== x "author-name") (make-author-data-element 'author-address))
	((== x "doc-inactive") (doc-data-activate-here))
	((in? x '("part" "part*" "chapter" "chapter*" "appendix"
		  "section" "subsection" "subsubsection"
		  "section*" "subsection*" "subsubsection*"
		  "paragraph" "subparagraph" "paragraph*" "subparagraph*"))
	 (return-sectional))
	((== x "item*") (go-end-of "item*"))
	((== x "equation") (go-end-of "equation") (insert-return))
	((== x "input") (session-return))
	((== x "table") (return-inside-table))
	((in? x '("itemize" "itemize-minus" "itemize-dot" "itemize-arrow"
		  "enumerate" "enumerate-numeric" "enumerate-roman"
		  "enumerate-Roman" "enumerate-alpha" "enumerate-Alpha"
		  "description" "description-compact" "description-aligned"
		   "description-dash" "description-long"))
	 (make-item))
	((inside? "compound") (activate-compound))
	(else (insert-return))))

(define (make-return)
  (make-return-inside
   (inside-which '("inactive" "latex" "hybrid" "symbol"
		   "title" "author" "doc-title" "author-name" "doc-inactive"
		   "part" "part*" "chapter" "chapter*" "appendix"
		   "section" "subsection" "subsubsection"
		   "section*" "subsection*" "subsubsection*"
		   "paragraph" "subparagraph" "paragraph*" "subparagraph*"
		   "item*" "equation" "equation*" "table" "input"
		   "itemize" "itemize-minus" "itemize-dot" "itemize-arrow"
		   "enumerate" "enumerate-numeric" "enumerate-roman"
		   "enumerate-Roman" "enumerate-alpha" "enumerate-Alpha"
		   "description" "description-compact" "description-aligned"
		   "description-dash" "description-long"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shift-return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-shift-return-inside x)
  (cond ((== x "input") (session-shift-return))
	(else (insert-return))))

(define (make-shift-return)
  (make-shift-return-inside
    (inside-which '("input"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hybrid-insert forward)
  (let ((x (inside-which '("table" "tree" "switch" "input" "hybrid"))))
    (cond ((== x "table")
	   (table-insert-column forward))
	  ((== x "tree")
	   (branch-insert forward))
	  ((== x "switch")
	   (switch-insert (if forward "after" "before")))
	  ((== x "input")
	   (session-fold-input))
	  ((== x "hybrid")
	   (activate-hybrid #t)))))

(define (structured-insert-left)
  (let ((x (inside-which '("table" "tree" "switch"
			   "inactive" "hybrid" "tuple" "attr" "input"))))
    (if (in? x '("table" "tree" "switch" "input" "hybrid"))
	(hybrid-insert #f)
	(insert-argument #f))))

(define (structured-insert-right)
  (let ((x (inside-which '("table" "tree" "switch"
			   "inactive" "hybrid" "tuple" "attr" "input"))))
    (if (in? x '("table" "tree" "switch" "input" "hybrid"))
	(hybrid-insert #t)
	(insert-argument #t))))

(define (structured-insert-up)
  (let ((x (inside-which '("table" "input"))))
    (cond ((== x "table")
	   (table-insert-row #f))
	  ((== x "input")
	   (session-insert-input-above)))))

(define (structured-insert-down)
  (let ((x (inside-which '("table" "input"))))
    (cond ((== x "table")
	   (table-insert-row #t))
	  ((== x "input")
	   (session-insert-input-below)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose deletions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (structured-remove forward?)
  (let ((x (inside-which '("input"))))
    (cond ((== x "input") (session-remove-input forward?))
	  (else (remove-structure-upwards)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (position-default) (cell-del-format ""))
(define (position-left) (cell-halign-left))
(define (position-right) (cell-halign-right))
(define (position-up) (cell-valign-up))
(define (position-down) (cell-valign-down))
(define (position-start) (noop))
(define (position-end) (noop))
(define (position-top) (noop))
(define (position-bottom) (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some multi-purpose actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (general-remove forward?)
  (cond ((selection-active-normal?) (clipboard-cut "primary"))
	((and (in-session?) (inside? "input")) (session-remove forward?))
	(else (remove-text forward?))))

(define (general-tab)
  (cond ((or (inside? "label") (inside? "reference")) (complete-try?) (noop))
        ((inside? "hybrid") (activate-hybrid #t))
        ((or (inside? "inactive") (in-source?)
	     (inside? "tuple") (inside? "attr"))
	 (insert-argument #t))
	((and (in-session?)
	      (plugin-supports-completions? (get-env "prog-language")))
	 (if (session-complete-try?) (noop)))
	((complete-try?) (noop))
	(else (set-message "Use M-tab in order to insert a tab" "tab"))))
