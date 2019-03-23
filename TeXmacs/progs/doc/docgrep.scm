
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc docgrep)
  (:use (doc help-funcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get scores for the different files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-score-sub file keyword-list)
  (with r (system-search-score (system->url file) keyword-list)
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

(define (build-doc-search-results keyword the-result)
  ($tmdoc
    ($tmdoc-title (replace "Search results for ``%1''" `(verbatim ,keyword)))
    ($when (null? the-result)
      (replace "No matches found for ``%1''." keyword))
    ($when (nnull? the-result)
      ($with highest-score (cdar the-result)
        ($description-aligned
          ($for (x the-result)
            ($describe-item
                ($inline (quotient (* (cdr x) 100) highest-score) "%")
                (let* ((path (car x))
                       (title (help-file-title path))
                       (text (if (null? title) (car x) title)))
                 ($link path text))
              '(htab "")
              ($ismall
                ($verbatim
                  (string-append " ("
                                 (cAr (string-tokenize-by-char (car x) #\/)))
                                 ")" )))))))))

(define (build-doc-link-page keyword file-list)
  (let* ((keyword-list (string-tokenize-by-char keyword #\space))
         (the-result (get-score-list keyword-list file-list)))
    (tm->stree (build-doc-search-results keyword the-result))))

(define (src-file-short-name s) 
  (let ((p1 (url->system (unix->url "$TEXMACS_PATH/")))
        (p2 (url->system (unix->url "$TEXMACS_SOURCE_PATH/"))))
    (cond ((nstring? s) s)  ;; wtf?
          ((string-starts? s p1) (string-drop s (string-length p1)))
          ((string-starts? s p2) (string-drop s (string-length p2)))
          (else s))))

(define (build-src-search-results keyword the-result)
  ($tmdoc
    ($tmdoc-title (replace "Search results for ``%1''" `(verbatim ,keyword)))
    ($when (null? the-result)
      (replace "No matches found for ``%1''." keyword))
    ($when (nnull? the-result)
      ($with highest-score (cdar the-result)
        ($description-aligned
          ($for (x the-result)
            ($describe-item
                ($inline (quotient (* (cdr x) 100) highest-score) "%")
              ($link (car x) (src-file-short-name (car x))))))))))

(define (build-src-link-page keyword file-list)
  (let* ((keyword-list (string-tokenize-by-char keyword #\space))
         (the-result (get-score-list keyword-list file-list)))
    (tm->stree (build-src-search-results keyword the-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find documentation or source in a given path and matching a given pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define url-collect-cache (make-ahash-table))

(define path-separator (if (or (os-mingw?) (os-win32?)) #\; #\:))

(define (url-collect path pattern)
  (or (ahash-ref url-collect-cache (string-append path pattern))
      (ahash-set! url-collect-cache (string-append path pattern)
                  (let* ((u (url-append (unix->url path) (url-any)))
                         (v (url-expand (url-complete u "dr")))
                         (w (url-append v (url-wildcard pattern)))
                         (x (url-expand (url-complete w "fr"))))
                    x))))

(define (docgrep what path . patterns)
  (let* ((l1 (map (lambda (pat) (url-collect path pat)) patterns))
         (l2 (map url->system l1))
         (l3 (append-map (cut string-tokenize-by-char <> path-separator) l2)))
    (build-doc-link-page what l3)))

; TODO: include results from the code indexer when available
(define (srcgrep what path . patterns)
  (let* ((l1 (map (lambda (pat) (url-collect path pat)) patterns))
         (l2 (map url->system l1))
         (l3 (append-map (cut string-tokenize-by-char <> path-separator) l2)))
    (build-src-link-page what l3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration in TeXmacs file system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (grep query)
  (let* ((type (query-ref query "type"))
         (what (query-ref query "what"))
         (lan  (string-take (language-to-locale (get-output-language)) 2)))
    (cond ((== type "Scheme")
           (srcgrep what "$TEXMACS_PATH/progs" "*.scm"))
          ((== type "Styles")
           (srcgrep what "$TEXMACS_PATH/styles:$TEXMACS_PATH/packages" "*.ts"))
          ((== type "C++")
           (srcgrep what "$TEXMACS_SOURCE_PATH/src" "*.hpp" "*.cpp"))
          ((== type "All code")
           (srcgrep what "$TEXMACS_PATH:$TEXMACS_SOURCE_PATH/src"
                    "*.scm" "*.hpp" "*.cpp" "*.ts"))
          ((== type "texts")
           (docgrep what "$TEXMACS_FILE_PATH" "*.tm"))
          ((== type "doc")
           (docgrep what "$TEXMACS_DOC_PATH"
            (string-append "*." lan ".tm")
            (string-append "*." lan ".tmml")))
          (else
           (docgrep what "$TEXMACS_DOC_PATH" "*.en.tm")))))

(tmfs-title-handler (grep query doc)
  (with what (query-ref query "what")
    (replace "Help - Search results for ``%1''" what)))

(tm-define (docgrep-in-doc what)
  (:argument what "Search words in the documentation")
  (with query (list->query (list (cons "type" "doc") (cons "what" what)))
    (load-buffer (string-append "tmfs://grep/" query))))

(tm-define (docgrep-in-src what where)
  (:argument what "Search words")
  (:argument where "In")
  (:proposals where '("Scheme" "Styles" "C++" "All code"))
  (with query (list->query (list (cons "type" where) (cons "what" what)))
    (load-buffer (string-append "tmfs://grep/" query))))

(tm-define (docgrep-in-texts what)
  (:argument what "Search words in my documents")
  (with query (list->query (list (cons "type" "texts") (cons "what" what)))
    (load-buffer (string-append "tmfs://grep/" query))))
