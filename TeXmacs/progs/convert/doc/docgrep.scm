
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : docgrep.scm
;; DESCRIPTION : grep words in the documentation or sources
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven, Michael Floc'hlay and
;;                         Arnaud Ebalard
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert doc docgrep)
  (:export docgrep-in-doc docgrep-in-src docgrep-in-texts))

;;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; FIXME: improvements to be made:
;; - Don't use tmpnam from Guile but eval-system from TeXmacs
;; - Directly jump to the document if exactly one occurrence is found
;; - Translations for generated text
;;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The function 'file-list-containing-keyword'
;; searches the given keyword in the given file list.
;; It returns a list of elements from the following pattern :
;;   (<filename> (set (match <line> <occurences> <text>)))
;; If nothing is found, it returns the empty-list.
;; For example, ("file.txt" (set (match 12 2 "wo wo"))) means that :
;; the keyword you're searching (the word "wo" in our example) is present twice
;; in the line 12 of "file.txt" and this line number 12 is "wo wo".
;; WARNING: this function uses temporary files !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-list-containing-keyword keyword file-list)
  (map (lambda (x)
	 (cons (car x)
	       (cons (string->object (cadr x))
		     (cddr x))))
       (map (lambda (x)
	      (string-tokenize-n x #\: 2))
	    (but-last
	     (string-tokenize
	      (eval-system (string-append "grep -w -n " keyword " " file-list))
	      #\newline)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function get-mode transforms a list of elements like :
;; (<filename> (set (match <num> <num> <text>)))
;; into a list of elements like :
;; (<filename> (set (match <num> <num> <mode>)))
;; where <mode> is either "mode-key" or "mode-simple" depending on whether the
;; <text> contains "judicious words" or not...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-before? s pos what)
  (and (>= pos (string-length what))
       (== (substring s (- pos (string-length what)) pos) what)))

(define (get-type-match token s filename pos)
  (cond ((string-ends? filename ".tm")
	 (cond ((string-before? s pos "<") "mode-invalid")
	       ((string-before? s pos "<\\") "mode-invalid")
	       ((string-before? s pos "<|") "mode-invalid")
	       ((string-before? s pos "</") "mode-invalid")
	       ((string-before? s pos "compound|") "mode-invalid")
	       ((string-before? s pos "<name|") "mode-key")
	       ((string-before? s pos "<tmstyle|") "mode-key")
	       ((string-before? s pos "<tmdtd|") "mode-key")
	       ((string-before? s pos "<explain-macro|") "mode-key")
	       (else "mode-simple")))
	((string-ends? filename ".scm")
	 (cond ((string-before? s pos "define ") "mode-key")
	       ((string-before? s pos "define (") "mode-key")
	       ((string-before? s pos "define-macro ") "mode-key")
	       ((string-before? s pos "define-macro (") "mode-key")
	       (else "mode-simple")))
	(else "mode-simple")))

(define (get-matches linenr token s filename pos)
  (with found (string-search-forwards token pos s)
    (if (< found 0) '()
	(with r (get-matches linenr token s filename (+ found 1))
	  (with type (get-type-match token s filename found)
	    (if (== type "mode-invalid") r
		(cons `(match ,linenr 1 ,type) r)))))))

(define (get-mode l token)
  (if (null? l) l
      (with r (get-mode (cdr l) token)
	(with (filename linenr s) (car l)
	  (with matches (get-matches linenr token s filename 0)
	    (if (null? matches) r
		(cons `(,filename (set ,@matches)) r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The argument of the function is a list of elements like :
;; (<file> (set match))
;; It returns a list where all the consecutive elements that have the same
;; <file> name have been "concatenated" into (<file> (set match1 match2 ...))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (group-by-filename l)
  (if (or (null? l) (null? (cdr l))) l
      (with r (group-by-filename (cdr l))
	(if (== (caar l) (caar r))
	    (cons `(,(caar l) (set ,@(append (cdadar l) (cdadar r)))) (cdr r))
	    (cons (car l) r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; token must be a string without any whitespaces
;; The function returns a list of elements like (<filename> (set [matches...]))
;; which represent all the occurences of the token we have found in file-list
;; No file must appear more than once in the returned list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-results-for-token token file-list)
  (with gm (get-mode (file-list-containing-keyword token file-list) token)
    ;(display* "gm= " gm "\n")
    (with ret (group-by-filename gm)
      ;(display* "ret= " ret "\n")
      ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This funcion returns the a list whose elements are the results of each token
;; of the string keyword.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-results-for-string keyword file-list)
  (map (lambda (x) (get-results-for-token x file-list))
       (string-tokenize keyword #\ )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basically, the 4 following functions make the intersection of lists using
;; hash-tables (the lists are identified by their first element)
;; It returns a new list where no identifying elements is present more than
;; once and where all the information of the different initial list are now
;; represented like (<file> and info1 info2 ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-results-for-files keyword file-list)
  (results-conjunction (get-results-for-string keyword file-list)))

(define (results-conjunction l)
  (cond ((null? l) l)
	((null? (cdr l))
	 (map (lambda (x) `(,(car x) . (and ,@(cdr x)))) (car l)))
	(else
	 (let* ((r (results-conjunction (cdr l)))
		(t (list->ahash-table
		    r)))
	   (results-conjunction-insert (car l) t)))))

(define (results-conjunction-insert l t)
  (if (null? l) l
      (let* ((x (caar l))
	     (y (ahash-ref t x))
	     (r (results-conjunction-insert (cdr l) t)))
	(if y
	    (cons `(,x . (and ,@(cdar l) ,@(cdr y))) r)
	    r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; l is a list of elements like (<file> and info1 info2 ...)
;; it returns a list which give a "score" (which is used to represent the fact
;; that a file may be more interesting than an other) for each file in the
;; following pattern (<file> . <score>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-pair-file-score l)
  (cons (car l) (get-score (cdr l))))

(define (get-score t)
 (let ((label (car t)))
   (cond ((equal? label 'and) (apply * (map get-score (cdr t))))
	  ((equal? label 'set) (apply + (map get-score (cdr t))))
	  ((equal? label 'match)
	   (let ((mode (cadddr t)))
	     (cond ((equal? mode "mode-simple") (caddr t))
		   ((equal? mode "mode-key") (* 25 (caddr t)))
		   (else (caddr t)))))
	  (else 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The argument of this function is a list of pairs whose second member is a
;; number. The function returns the same list where all its elements are sorted
;; by this second member.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sort-by-second-member l)
  (let aux1 ((list_to_be_sort l)
	     (acc '()))
    (letrec ((aux2 (lambda (pair_to_insert list1 list2)
		     (if (null? list2)
			 (append list1 (list pair_to_insert))
			 (if (> (cdr pair_to_insert) (cdar list2))
			     (append list1 (list pair_to_insert) list2)
			     (aux2 pair_to_insert
				   (append list1 (list (car list2)))
				   (cdr list2)))))))
      (if (null? list_to_be_sort)
	  acc
	  (aux1 (cdr list_to_be_sort)
		(aux2 (car list_to_be_sort) '() acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function returns a list of elements (<file> . <score>) sorted by the
;; score field (the highest score first), which represent the "final" results
;; for searching the given tokens of keyword in the file-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-final-sorted-results keyword file-list)
  (sort-by-second-member (map get-pair-file-score
			      (get-results-for-files keyword file-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build-link-page sets a new buffer help with hyper-links on files which
;; contain each token of the keyword string. The most appropriate files
;; occur on top of the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-hlink l highest-score)
  `(concat (item* (concat
		   ,(object->string (quotient (* (cdr l) 100) highest-score))
		   "%"))
	   (hlink ,(cAr (string-tokenize (car l) #\/)) ,(car l))))

(define (make-translate . args)
  (with s (apply string-append args)
    `(translate ,s "english" (value "language"))))

(define (build-link-page keyword file-list)
  (let* ((lan (get-output-language))
	 (the-result (get-final-sorted-results keyword file-list))
	 (text (make-translate "No matches found for#\`\`" keyword "''."))
	 (body (list text)))
    (if (not (null? the-result))
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
				  "Results of the search for#\`\`"
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

(define (urls->string l)
  (if (null? l) ""
      (with r (urls->string (cdr l))
	(if (url-none? (car l)) r
	    (with s (string-replace (url->string (car l)) ":" " ")
	      (if (== r "") s (string-append s " " r)))))))

(define (docgrep what path . patterns)
  (with l (map (lambda (pat) (url-collect path pat)) patterns)
    (build-link-page what (urls->string l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docgrep-in-doc what)
  (with lan (get-output-language)
    (cond ((== lan "french")
	   (docgrep what "$TEXMACS_DOC_PATH:$TEXMACS_PATH/doc" "*.fr.tm"))
	  ((== lan "spanish")
	   (docgrep what "$TEXMACS_DOC_PATH:$TEXMACS_PATH/doc" "*.es.tm"))
	  (else
	   (docgrep what "$TEXMACS_DOC_PATH:$TEXMACS_PATH/doc" "*.en.tm")))))

(define (docgrep-in-src what)
  (docgrep what "$TEXMACS_PATH/progs:$TEXMACS_SOURCE_PATH/src"
	   "*.scm" "*.hpp" "*.cpp"))

(define (docgrep-in-texts what)
  (docgrep what "$TEXMACS_FILE_PATH" "*.tm"))
