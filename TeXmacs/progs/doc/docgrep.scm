
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
  (with r (system-search-score (url-system file) keyword-list)
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
      "Search results for ``" keyword "''")
    ($when (null? the-result)
      "No matches found for ``" keyword "''.")
    ($when (nnull? the-result)
      ($with highest-score (cdar the-result)
        ($description-aligned
          ($for (x the-result)
            ($describe-item
                ($inline (quotient (* (cdr x) 100) highest-score) "%")
              ($link (car x)
                (cAr (string-tokenize-by-char (car x) #\/))))))))))

(define (build-link-page keyword file-list)
  (let* ((keyword-list (string-tokenize-by-char keyword #\space))
	 (the-result (get-score-list keyword-list file-list)))
    (tm->stree (build-search-results keyword the-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find documentation in given path and matching a given pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define path-separator (if (or (os-mingw?) (os-win32?)) #\; #\:))

(define (url-collect path pattern)
  (let* ((u (url-append (string->url path) (url-any)))
	 (v (url-expand (url-complete u "dr")))
	 (w (url-append v (url-wildcard pattern)))
	 (x (url-expand (url-complete w "fr"))))
    x))

(define (docgrep what path . patterns)
  (let* ((l1 (map (lambda (pat) (url-collect path pat)) patterns))
	 (l2 (map url->string l1))
	 (l3 (append-map (cut string-tokenize-by-char <> path-separator) l2)))
    (build-link-page what l3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration in TeXmacs file system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (grep query)
  (let* ((type (query-ref query "type"))
         (what (query-ref query "what"))
         (lan  (get-output-language)))
    (cond ((== type "src")
           (docgrep what "$TEXMACS_PATH/progs:$TEXMACS_SOURCE_PATH/src"
                    "*.scm" "*.hpp" "*.cpp"))
          ((== type "texts")
           (docgrep what "$TEXMACS_FILE_PATH" "*.tm"))
          ((and (== lan "french") (== type "doc"))
 	   (docgrep what "$TEXMACS_DOC_PATH" "*.fr.tm"))
 	  ((and (== lan "german") (== type "doc"))
 	   (docgrep what "$TEXMACS_DOC_PATH" "*.de.tm"))
 	  ((and (== lan "italian") (== type "doc"))
 	   (docgrep what "$TEXMACS_DOC_PATH" "*.it.tm"))
 	  ((and (== lan "spanish") (== type "doc"))
 	   (docgrep what "$TEXMACS_DOC_PATH" "*.es.tm"))
 	  ((and (== lan "portuguese") (== type "doc"))
 	   (docgrep what "$TEXMACS_DOC_PATH" "*.pt.tm"))
 	  (else
 	   (docgrep what "$TEXMACS_DOC_PATH" "*.en.tm")))))

(tmfs-name-handler (grep query)
  (with what (query-ref query "what")
    (string-append "Help - Search results for ``" what "''")))

(tm-define (docgrep-in-doc what)
  (:argument what "Search words in the documentation")
  (with query (list->query (list (cons "type" "doc") (cons "what" what)))
    (load-buffer (string-append "tmfs://grep/" query))))

(tm-define (docgrep-in-src what)
  (:argument what "Search words in the source code")
  (with query (list->query (list (cons "type" "src") (cons "what" what)))
    (load-buffer (string-append "tmfs://grep/" query))))

(tm-define (docgrep-in-texts what)
  (:argument what "Search words in my documents")
  (with query (list->query (list (cons "type" "texts") (cons "what" what)))
    (load-buffer (string-append "tmfs://grep/" query))))
