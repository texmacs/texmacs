
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-menu.scm
;; DESCRIPTION : extra menus for editing bibliographic databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-menu)
  (:use (database db-menu)
        (database bib-manage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve kind of bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-kind)
  (:mode in-bib?)
  "bib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra routines for entering names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-bib-names% (or (inside-db-field? "author") (inside-db-field? "editor"))
                 in-bib%))

(tm-define (make-name-sep)
  (when (inside? 'name)
    (go-end-of 'name))
  (when (inside? 'name-von)
    (go-end-of 'name-von))
  (when (inside? 'name-jr)
    (go-end-of 'name-jr))
  (make 'name-sep))

(tm-define (insert-name-von von)
  (when (not (inside? 'name-von))
    (when (inside? 'name)
      (go-start-of 'name))
    (when (inside? 'name-jr)
      (go-start-of 'name-jr))
    (insert-go-to `(concat (name-von ,von) " ") '(1 1))))

(tm-define (make-name-von)
  (when (not (inside? 'name-von))
    (when (inside? 'name)
      (go-start-of 'name)
      (insert-go-to " " '(0)))
    (when (inside? 'name-jr)
      (go-start-of 'name-jr)
      (insert-go-to " " '(0)))
    (make 'name-von)))

(tm-define (insert-name-jr jr)
  (when (not (inside? 'name-jr))
    (when (inside? 'name)
      (go-end-of 'name))
    (when (inside? 'name-von)
      (go-end-of 'name-von))
    (insert-go-to `(concat " " (name-jr ,jr)) '(1 1))))

(tm-define (make-name-jr)
  (when (not (inside? 'name-jr))
    (when (inside? 'name)
      (go-end-of 'name)
      (insert-go-to " " '(1)))
    (when (inside? 'name-von)
      (go-end-of 'name-von)
      (insert-go-to " " '(1)))
    (make 'name-jr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra menu items for entering names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind bib-von-menu
  ("de" (insert-name-von "de"))
  ("van" (insert-name-von "van"))
  ("von" (insert-name-von "von"))
  ("zu" (insert-name-von "zu"))
  ---
  ("Other" (make-name-von)))

(menu-bind bib-jr-menu
  ("Junior" (insert-name-jr "Jr."))
  ("Senior" (insert-name-jr "Sr."))
  ---
  ("Other" (make-name-jr)))

(menu-bind db-extra-menu
  (:mode in-bib-names?)
  ---
  (-> "Particle" (link bib-von-menu))
  ("Last name" (make 'name))
  (-> "Title suffix" (link bib-jr-menu))
  ("Extra name" (make 'name-sep)))

(menu-bind db-extra-icons
  (:mode in-bib-names?)
  /
  (=> (balloon (icon "tm_von.xpm") "Insert particle")
      (link bib-von-menu))
  ((balloon (icon "tm_name_bis.xpm") "Insert last name")
   (make 'name))
  (=> (balloon (icon "tm_junior.xpm") "Insert title after name")
      (link bib-jr-menu))
  ((balloon (icon "tm_and.xpm") "Insert more names")
   (make-name-sep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening focus search tool for citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-search? t)
  (:require (and (supports-db?) (bib-cite-context? t)))
  #t)

(tm-define (focus-open-search-tool t)
  (:require (and (supports-db?) (bib-cite-context? t)))
  (and-with u (if (tree-func? t 'cite-detail) (tree-ref t 0) (tree-down t))
    (open-bib-chooser
     (lambda (key)
       (when (and (tree->path u)
		  (tree-in? (tree-up u)
                            '(cite nocite cite-detail cite-TeXmacs)))
	 (tree-set! u key))))))
