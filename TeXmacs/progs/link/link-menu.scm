
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-menu.scm
;; DESCRIPTION : menus for linking portions of text
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-menu)
  (:use (link link-edit) (link link-navigate) (link link-extract)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering of loci
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-locus-rendering? var val)
  (== (get-locus-rendering var) val))
(tm-define (change-locus-rendering var val)
  (:synopsis "Change global locus rendering property @var to @val.")
  (:check-mark "v" test-locus-rendering?)
  (set-locus-rendering var val)
  (update-all-buffers))

(define (test-locus-preserve-on-paper?)
  (== (get-locus-rendering "locus-on-paper") "preserve"))
(tm-define (toggle-locus-preserve-on-paper)
  (:synopsis "Toggle whether loci are colored when rendering on paper.")
  (:check-mark "v" test-locus-preserve-on-paper?)
  (with val (if (test-locus-preserve-on-paper?) "change" "preserve")
    (change-locus-rendering "locus-on-paper" val)))

(tm-define (interactive-change-locus-rendering var)
  (:interactive #t)
  (interactive (lambda (val) (change-locus-rendering var val))
    (list (logic-ref env-var-description% var) "string"
	  (get-locus-rendering var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (link-create-menu)
  (let* ((l1 (current-link-types))
	 (l2 (list-remove-duplicates (cons* "hyperlink" "action" l1)))
	 (l3 (list-sort l2 string<=?)))
    (for (name l3) ((eval name) (make-link name)))
    ---
    ("Other" (interactive make-link))))

(tm-menu (link-delete-menu)
  (let* ((l1 (locus-link-types #t))
	 (l2 (list-sort l1 string<=?)))
    ("All" (remove-all-links))
    ---
    (for (name l2) ((eval name) (remove-link-of-types name)))
    ---
    ("Other" (interactive remove-link-of-types))))

(tm-menu (navigation-type-menu)
  (let* ((l1 (current-link-types))
	 (l2 (list-sort l1 string<=?)))
    (for (name l2)
      ((eval name)
       (navigation-toggle-type name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main link menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind link-menu
  ("New locus" (make-locus))
  ---
  (-> "Link mode"
      ("Simple" (set-link-mode "simple"))
      ("Bidirectional" (set-link-mode "bidirectional"))
      ("External" (set-link-mode "external")))
  (when (inside? 'locus)
    ("Source" (link-set-locus 0)))
  (-> "Target"
      (when (inside? 'locus)
	("Locus" (link-set-locus 1)))
      ("Url" (check "o" (link-target-is-url?))
       (interactive link-set-target-url))
      ("Script" (check "o" (link-target-is-script?))
       (interactive link-set-target-script)))
  (when (link-completed?)
    (-> "Create link" (link link-create-menu)))
  (if (null? (locus-link-types #t))
      (when #f
	("Delete link" (noop))))
  (if (nnull? (locus-link-types #t))
    (-> "Delete link" (link link-delete-menu)))
  ---
  (-> "Locus rendering"
      ("Disable coloring on paper" (toggle-locus-preserve-on-paper))
      (-> "Normal loci"
	  ("Default" (change-locus-rendering "locus-color" "#404080"))
	  ("Preserve" (change-locus-rendering "locus-color" "preserve"))
	  ("Blue" (change-locus-rendering "locus-color" "dark blue"))
	  ("Red" (change-locus-rendering "locus-color" "red"))
	  ---
	  ("Other" (interactive-change-locus-rendering "locus-color")))
      (-> "Visited loci"
	  ("Default" (change-locus-rendering "visited-color" "#702070"))
	  ("Preserve" (change-locus-rendering "visited-color" "preserve"))
	  ("Magenta" (change-locus-rendering "visited-color" "dark magenta"))
	  ("Red" (change-locus-rendering "visited-color" "red"))
	  ---
	  ("Other" (interactive-change-locus-rendering "visited-color"))))
  (-> "Navigation options"
      ("Follow inverse links" (navigation-toggle-bidirectional))
      ("Follow external links" (navigation-toggle-external))
      ("Build link pages" (navigation-toggle-build-link-pages)))
  (-> "Active link types"
      ("None" (navigation-allow-no-types))
      ("All" (navigation-allow-all-types))
      (if (nnull? (current-link-types))
	  ---
	  (link navigation-type-menu)))
  (-> "Extract"
      ("Constellation" (build-constellation-page))
      ("Loci" (build-locus-page))
      ("Environments" (interactive build-environment-page))))
