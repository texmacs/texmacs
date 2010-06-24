
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrv.scm
;; DESCRIPTION : abbrv style for BibTeX files
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex abbrv)
  (:use (bibtex bib-utils)))

(bib-define-style "abbrv" "plain")

(tm-define (format-name x)
  (:mode bib-abbrv?)
  (let* ((f (if (empty? (list-ref x 1))
		""
		`(concat ,(bib-abbreviate (list-ref x 1) "." `(nbsp))
			 (nbsp))))
	 (vv (if (empty? (list-ref x 2)) "" `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (empty? (list-ref x 3)) "" (list-ref x 3)))
	 (jj (if (empty? (list-ref x 4)) "" `(concat ", " ,(list-ref x 4)))))
    `(concat ,f ,vv ,ll ,jj)))

(tm-define (format-pages x)
  (:mode bib-abbrv?)
  (let* ((p (bib-field x "pages")))
    (cond
     ((equal? 1 (length p)) "")
     ((equal? 2 (length p)) `(concat "pp. " ,(list-ref p 1)))
     (else `(concat "pp. " ,(list-ref p 1) "--" ,(list-ref p 2))))))

