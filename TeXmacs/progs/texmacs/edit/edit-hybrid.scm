
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
    (generic generic-edit)
    (text text-edit) (text title-edit)
    (table table-edit) (graphics graphics-edit)
    (dynamic session-edit) (dynamic fold-edit)))

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

(tm-define (structured-insert-left)
  (let ((x (inside-which '("table" "tree" "switch"
			   "inactive" "hybrid" "tuple" "attr" "input"))))
    (if (in? x '("table" "tree" "switch" "input" "hybrid"))
	(hybrid-insert #f)
	(insert-argument #f))))

(tm-define (structured-insert-right)
  (let ((x (inside-which '("table" "tree" "switch"
			   "inactive" "hybrid" "tuple" "attr" "input"))))
    (if (in? x '("table" "tree" "switch" "input" "hybrid"))
	(hybrid-insert #t)
	(insert-argument #t))))

(tm-define (structured-insert-up)
  (let ((x (inside-which '("table" "input"))))
    (cond ((== x "table")
	   (table-insert-row #f))
	  ((== x "input")
	   (session-insert-input-above)))))

(tm-define (structured-insert-down)
  (let ((x (inside-which '("table" "input"))))
    (cond ((== x "table")
	   (table-insert-row #t))
	  ((== x "input")
	   (session-insert-input-below)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose deletions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-remove forward?)
  (let ((x (inside-which '("input"))))
    (cond ((== x "input") (session-remove-input forward?))
	  (else (remove-structure-upwards)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (position-default) (cell-del-format ""))
(tm-define (position-left) (cell-halign-left))
(tm-define (position-right) (cell-halign-right))
(tm-define (position-up) (cell-valign-up))
(tm-define (position-down) (cell-valign-down))
(tm-define (position-start) (noop))
(tm-define (position-end) (noop))
(tm-define (position-top) (noop))
(tm-define (position-bottom) (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some multi-purpose actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (general-remove forward?)
  (cond ((selection-active-normal?) (clipboard-cut "primary"))
	((and (in-session?) (inside? "input")) (session-remove forward?))
	(else (remove-text forward?))))

(tm-define (general-tab)
  (cond ((or (inside? "label") (inside? "reference")) (complete-try?) (noop))
        ((inside? "hybrid") (activate-hybrid #t))
        ((or (inside? "inactive") (in-source?)
	     (inside? "tuple") (inside? "attr"))
	 (insert-argument #t))
	((and (in-session?)
	      (plugin-supports-completions? (get-env "prog-language")))
	 (if (session-complete-try?) (noop)))
	((complete-try?) (noop))
	((in-graphics?) (graphics-choose-point))
	(else (set-message "Use M-tab in order to insert a tab" "tab"))))
