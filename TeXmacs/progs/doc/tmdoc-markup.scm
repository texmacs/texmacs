
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-markup.scm
;; DESCRIPTION : markup for documentation mode
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-markup))

(tm-define (tmdoc-key s)
  (:secure #t)
  (lazy-keyboard-force #t)
  (cond ((string? s)
	 (let* ((s2 (kbd-pre-rewrite s))
		(s3 (kbd-post-rewrite s2 #f)))
	   (kbd-system-rewrite s3)))
	((tree? s) (tmdoc-key (tree->stree s)))
	(else '(render-key (with "color" "red" "?")))))

(tm-define (tmdoc-key* s)
  (:secure #t)
  (lazy-keyboard-force #t)
  (cond ((string? s) (kbd-system-rewrite s))
	((tree? s) (tmdoc-key* (tree->stree s)))
	(else '(render-key (with "color" "red" "?")))))

(tm-define (tmdoc-shortcut s)
  (:secure #t)
  (lazy-keyboard-force #t)
  (cond ((string? s)
	 (with r (kbd-find-rev-binding s)
	   (tmdoc-key r)))
	((tree? s) (tmdoc-shortcut (tree->stree s)))
	(else '(render-key (with "color" "red" "?")))))
