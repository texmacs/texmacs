
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-menu.scm
;; DESCRIPTION : linking portions of text
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-menu)
  (:use (link link-edit) (link link-navigate) (link link-extract)
	(generic document-edit)))

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
  (list name (lambda () (remove-link name))))

(tm-define (link-delete-menu)
  (let* ((l1 (locus-link-types #t))
	 (l2 (list-sort l1 string<=?)))
    (menu-dynamic
      ("All" (remove-all-links))
      ---
      ,@(map link-delete-entry l2)
      ---
      ("Other" (interactive remove-link)))))

(define (navigation-type-entry name)
  (list name (eval `(lambda () (navigation-toggle-type ,name)))))

(tm-define (navigation-type-menu)
  (let* ((l1 (current-link-types))
	 (l2 (list-sort l1 string<=?)))
    (menu-dynamic
      ,@(map navigation-type-entry l2))))

(menu-bind link-menu
  ("New locus" (make-locus))
  ---
  (-> "Link mode"
      ("Simple" (set-link-mode "simple"))
      ("Bidirectional" (set-link-mode "bidirectional"))
      ("External" (set-link-mode "external")))
  (when (inside? 'locus)
    ("Source" (link-set-locus 0))
    ("Target" (link-set-locus 1)))
  (when (link-completed-loci?)
    (-> "Create link" (link link-create-menu)))
  (if (null? (locus-link-types #t))
      (when #f
	("Delete link" (noop))))
  (if (nnull? (locus-link-types #t))
    (-> "Delete link" (link link-delete-menu)))
  ---
  (-> "Locus rendering"
      ("Default" (init-default "locus-color"))
      ---
      ("Preserve" (init-env "locus-color" "preserve"))
      ("Steel blue" (init-env "locus-color" "#404080"))
      ("Dark blue" (init-env "locus-color" "dark blue"))
      ("Red" (init-env "locus-color" "red"))
      ---
      ("Other" (init-interactive-env "locus-color")))
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
      ("Loci" (build-locus-page))
      ("Environment" (interactive build-environment-page))))
