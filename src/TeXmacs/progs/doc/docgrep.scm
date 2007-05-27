
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
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

(define (make-hlink l highest-score)
  `(concat (item* (concat
		   ,(object->string (quotient (* (cdr l) 100) highest-score))
		   "%"))
	   (hlink ,(cAr (string-tokenize-by-char (car l) #\/)) ,(car l))))

(define (make-translate . args)
  (with s (apply string-append args)
    `(translate ,s "english" (value "language"))))

(define (build-link-page keyword file-list)
  (let* ((lan (get-output-language))
	 (keyword-list (string-tokenize-by-char keyword #\space))
	 (the-result (get-score-list keyword-list file-list))
	 ;;(the-result (get-final-sorted-results keyword file-list))
	 (text (make-translate "No matches found for ``" keyword "''."))
	 (body (list text)))
    (if (nnull? the-result)
	(let ((highest-score (cdar the-result)))
	  (set! body (map (lambda (x) (make-hlink x highest-score))
			  the-result))
	  (set! body `((description-aligned (document ,@body))))))
    (set-help-buffer "Results of search"
		     `(document
		       (style "tmdoc")
		       (body (document
			      (tmdoc-title
			       (concat
				,(make-translate
				  "Results of the search for ``"
				  keyword
				  "''")))
			      ,@body))
		       (initial (collection (associate "language" ,lan)))))))

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
