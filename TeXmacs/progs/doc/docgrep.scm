
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : docgrep.scm
;; DESCRIPTION : grep words in the documentation or sources
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven, Michael Floc'hlay and
;;                         Arnaud Ebalard
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: improvements to be made:
;; - Directly jump to the document if exactly one occurrence is found
;; - Launch a search when jumping to a matching document
;; - Translations for generated text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc docgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get scores for the different files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-score-sub file keyword-list)
  (with r (system-search-score (string->url file) keyword-list)
    (cons file r)))

(define (get-score-list keyword-list file-list)
  (let* ((l1 (map (cut get-score-sub <> keyword-list) file-list))
	 (l2 (list-filter l1 (lambda (x) (!= (cdr x) 0))))
	 (l3 (list-sort l2 (lambda (x y) (>= (cdr x) (cdr y))))))
    l3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build-link-page sets a new buffer help with hyper-links on files which
;; contain each token of the keyword string. The most appropriate files
;; occur on top of the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-search-results keyword the-result)
  ($tmdoc
    ($tmdoc-title
      ($localize "Search results for ``" keyword "''"))
    ($when (null? the-result)
      ($localize "No matches found for ``" keyword "''."))
    ($when (nnull? the-result)
      ($with highest-score (cdar the-result)
        $lf
        ($description-aligned
          ($for (x the-result)
            ($describe-item
              ($inline (quotient (* (cdr x) 100) highest-score) "%")
              ($link (car x)
                (cAr (string-tokenize-by-char (car x) #\/))))))))))

(define (build-link-page keyword file-list)
  (let* ((keyword-list (string-tokenize-by-char keyword #\space))
	 (the-result (get-score-list keyword-list file-list)))
    (set-help-buffer "Results of search"
		     (build-search-results keyword the-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find documentation in given path and matching a given pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (url-collect path pattern)
  (let* ((u (url-append (string->url path) (url-any)))
	 (v (url-expand (url-complete u "dr")))
	 (w (url-append v (url-wildcard pattern)))
	 (x (url-expand (url-complete w "fr"))))
    x))

(define (docgrep what path . patterns)
  (let* ((l1 (map (lambda (pat) (url-collect path pat)) patterns))
	 (l2 (map url->string l1))
	 (l3 (append-map (cut string-tokenize-by-char <> #\:) l2)))
    (build-link-page what l3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (docgrep-in-doc what)
  (:argument what "Search words in the documentation")
  (with lan (get-output-language)
    (cond ((== lan "french")
	   (docgrep what "$TEXMACS_DOC_PATH" "*.fr.tm"))
	  ((== lan "german")
	   (docgrep what "$TEXMACS_DOC_PATH" "*.de.tm"))
	  ((== lan "italian")
	   (docgrep what "$TEXMACS_DOC_PATH" "*.it.tm"))
	  ((== lan "spanish")
	   (docgrep what "$TEXMACS_DOC_PATH" "*.es.tm"))
	  ((== lan "portuguese")
	   (docgrep what "$TEXMACS_DOC_PATH" "*.pt.tm"))
	  (else
	   (docgrep what "$TEXMACS_DOC_PATH" "*.en.tm")))))

(tm-define (docgrep-in-src what)
  (:argument what "Search words in the source code")
  (docgrep what "$TEXMACS_PATH/progs:$TEXMACS_SOURCE_PATH/src"
	   "*.scm" "*.hpp" "*.cpp"))

(tm-define (docgrep-in-texts what)
  (:argument what "Search words in my documents")
  (docgrep what "$TEXMACS_FILE_PATH" "*.tm"))
