
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : help.scm
;; DESCRIPTION : loading help files
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-help)
  (:use (texmacs texmacs tm-files))
  (:export
    url-exists-in-help? load-help-online update-help-online
    load-help-buffer load-help-article load-help-book))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading help buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define help-file-path
  "$TEXMACS_DOC_PATH:$TEXMACS_HOME_PATH/doc:$TEXMACS_PATH/doc")

(define (url-exists-in-help? s)
  (url-exists? (url help-file-path s)))

(define (url-resolve-help s)
  (if (or (in? (url-suffix s) '("tex" "tm")) (url-exists? s))
      s
      (let* ((lan (get-output-language))
	     (suf (cond ((== lan "czech") ".cs.tm")
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
	      ((and (not (== suf ".en.tm"))
		    (url-exists? (url dir (string-append s ".en.tm"))))
	       (url-resolve (url dir (string-append s ".en.tm")) "r"))
	      (else (url-none))))))

(define (load-help-buffer-sub s type)
  (let ((name (url-resolve-help s)))
    (if (url-none? name)
	(set-message (string-append "Error: help file#'" s "'#not found")
		     "load help file")
	(cond ((== type "normal")
	       (let ((doc (texmacs-load-tree name "help")))
		 (if (== (tree->object doc) "error")
		     (set-message "Bad help file" "load help file")
		     (set-help-buffer name doc))))
	      ((== type "article") (tmdoc-expand-help name 'tmdoc-title))
	      ((== type "book") (tmdoc-expand-help-manual name))))))

(define (load-help-buffer s) (load-help-buffer-sub s "normal"))
(define (load-help-article s) (load-help-buffer-sub s "article"))
(define (load-help-book s) (load-help-buffer-sub s "book"))

(define (load-help-online s)
  (load-help-buffer (url-append "http://www.gnu.org/software/texmacs-doc" s)))

(define (update-help-online)
  (system "cd $TEXMACS_HOME_PATH; wget ftp://ftp.texmacs.org/pub/TeXmacs/doc/TeXmacs-doc.tar.gz -O TeXmacs-doc.tar.gz; gunzip TeXmacs-doc.tar.gz; tar -xvf TeXmacs-doc.tar; rm -f TeXmacs-doc.tar"))
