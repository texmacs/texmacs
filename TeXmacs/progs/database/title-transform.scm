
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : title-transform.scm
;; DESCRIPTION : Title transformations for various presentation styles
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List all authors into a single author field and create references
;; for the individual author information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (single-author-list t)
  (:secure #t)
  (with authors (select t '(doc-author))
    (if (<= (length authors) 1) t t)))
