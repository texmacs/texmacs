
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : help-funcs.scm
;; DESCRIPTION : loading help files
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc help-funcs)
  (:use (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading help buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define help-file-path "$TEXMACS_DOC_PATH")

(tm-define (url-exists-in-help? s)
  (url-exists? (url help-file-path s)))

(define (url-resolve-help s)
  (if (or (in? (url-suffix s) '("tex" "tm")) (url-exists? s))
      s
      (let* ((lan (get-output-language))
	     (suf (cond ((== lan "bulgarian") ".bg.tm")
			((== lan "czech") ".cs.tm")
			((== lan "danish") ".da.tm")
			((== lan "dutch") ".nl.tm")
			((== lan "finnish") ".fi.tm")
			((== lan "french") ".fr.tm")
			((== lan "german") ".de.tm")
			((== lan "hungarian") ".hu.tm")
			((== lan "italian") ".it.tm")
			((== lan "polish") ".pl.tm")
			((== lan "portuguese") ".pt.tm")
			((== lan "romanian") ".ro.tm")
			((== lan "russian") ".ru.tm")
			((== lan "spanish") ".es.tm")
			((== lan "slovene") ".sl.tm")
			((== lan "swedish") ".sv.tm")
			((== lan "ukrainian") ".uk.tm")
			(else ".en.tm")))
	     (dir help-file-path))
	(cond ((url-exists? (url dir (string-append s suf)))
	       (url-resolve (url dir (string-append s suf)) "r"))
	      ((and (!= suf ".en.tm")
		    (url-exists? (url dir (string-append s ".en.tm"))))
	       (url-resolve (url dir (string-append s ".en.tm")) "r"))
	      (else (url-none))))))

(define (load-help-buffer-sub s type)
  (let ((name (url-resolve-help s)))
    (if (url-none? name)
	(set-message `(concat "Error: help file " (verbatim ,s) " not found")
		     "load help file")
	(cond ((== type "normal")
	       (let ((doc (texmacs-load-tree name "help")))
		 (if (== (tree->stree doc) "error")
		     (set-message "Bad help file" "load help file")
		     (set-help-buffer name doc))))
	      ((== type "article") (tmdoc-expand-help name 'tmdoc-title))
	      ((== type "book") (tmdoc-expand-help-manual name))))))

(tm-define (load-help-buffer s) (load-help-buffer-sub s "normal"))
(tm-define (load-help-article s) (load-help-buffer-sub s "article"))
(tm-define (load-help-book s) (load-help-buffer-sub s "book"))

(tm-define (load-help-online s)
  (load-help-buffer (url-append "http://www.texmacs.org/tmbrowse" s)))

(tm-define (update-help-online)
  (system "cd $TEXMACS_HOME_PATH; wget ftp://ftp.texmacs.org/pub/TeXmacs/doc/TeXmacs-doc.tar.gz -O TeXmacs-doc.tar.gz; gunzip TeXmacs-doc.tar.gz; tar -xvf TeXmacs-doc.tar; rm -f TeXmacs-doc.tar"))
