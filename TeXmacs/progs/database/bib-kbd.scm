
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-kbd.scm
;; DESCRIPTION : keyboard shortcuts for editing bibliographic databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-edit)
  (:use (database db-edit)
        (database bib-db)
        (database bib-widgets)))

(texmacs-modes
  (in-bib-names% (or (inside-db-field? "author") (inside-db-field? "editor"))
                 in-bib%))

(define (make-name-sep)
  (when (inside? 'name)
    (go-end-of 'name))
  (when (inside? 'name-von)
    (go-end-of 'name-von))
  (when (inside? 'name-jr)
    (go-end-of 'name-jr))
  (make 'name-sep))

(kbd-map
  (:mode in-bib-names?)
  ("," (make-name-sep))
  (", var" ",")
  ("space a n d space" (make-name-sep)))

(tm-define (kbd-alternate-variant t forwards?)
  (:require (and (supports-sql?)
		 (tree-in? t '(cite nocite cite-detail))))
  (and-with u (tree-down t)
    (open-bib-chooser
     (lambda (key)
       (when (and (tree->path u)
		  (tree-in? (tree-up u) '(cite nocite cite-detail)))
	 (tree-set! u key))))))
