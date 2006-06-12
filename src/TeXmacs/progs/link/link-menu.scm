
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-menu.scm
;; DESCRIPTION : menus for linking portions of text
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
    (list (drd-ref env-var-description% var) "string"
	  (get-locus-rendering var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (link-create-entry name)
  (list name (lambda () (make-link name))))

(tm-define (link-create-menu)
  (let* ((l1 (current-link-types))
	 (l2 (list-remove-duplicates (cons "standard" l1)))
	 (l3 (list-sort l2 string<=?)))
    (menu-dynamic
      ,@(map link-create-entry l3)
      ---
      ("Other" (interactive make-link)))))

(define (link-delete-entry name)
  (list name (lambda () (remove-link-of-types name))))

(tm-define (link-delete-menu)
  (let* ((l1 (locus-link-types #t))
	 (l2 (list-sort l1 string<=?)))
    (menu-dynamic
      ("All" (remove-all-links))
      ---
      ,@(map link-delete-entry l2)
      ---
      ("Other" (interactive remove-link-of-types)))))

(define (navigation-type-entry name)
  (list name (eval `(lambda () (navigation-toggle-type ,name)))))

(tm-define (navigation-type-menu)
  (let* ((l1 (current-link-types))
	 (l2 (list-sort l1 string<=?)))
    (menu-dynamic
      ,@(map navigation-type-entry l2))))

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
;;  (when (inside? 'locus)
;;    ("Source" (link-set-locus 0))
;;    ("Target" (link-set-locus 1)))
;;  (-> "Source"
;;      (when (inside? 'locus)
;;	("Locus" (link-set-locus 0)))
;;      ("Url" (interactive link-set-source-url))
;;      ("Script" (interactive link-set-source-script)))
  (when (inside? 'locus)
    ("Source" (link-set-locus 0)))
  (-> "Target"
      (when (inside? 'locus)
	("Locus" (link-set-locus 1)))
      ("Url" (interactive link-set-target-url))
      ("Script" (interactive link-set-target-script)))
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
	  ("Dark blue" (change-locus-rendering "locus-color" "dark blue"))
	  ("Red" (change-locus-rendering "locus-color" "red"))
	  ---
	  ("Other" (interactive-change-locus-rendering "locus-color")))
      (-> "Visited loci"
	  ("Default" (change-locus-rendering "visited-color" "#702070"))
	  ("Preserve" (change-locus-rendering "visited-color" "preserve"))
	  ("Magenta" (change-locus-rendering "visited-color" "magenta"))
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
