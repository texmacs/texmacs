
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : plain.scm
;; DESCRIPTION : plain style for BibTeX files
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To translate (except in french):
;; "edition"
;; "editor"
;; "editors"
;; "master's thesis"
;; "in"
;; "number" ???
;; "of"
;; "pages"
;; "phd thesis"
;; "technical report"
;; "volume"

(texmacs-module (bibtex plain)
  (:use (bibtex bib-utils)))

(bib-define-style "plain" "plain")

(tm-define (bib-preprocessing t) (:mode bib-plain?) `())

(tm-define (format-name x)
  (:mode bib-plain?)
  (let* ((ff (if (empty? (list-ref x 1)) "" `(concat ,(list-ref x 1) (nbsp))))
	 (vv (if (empty? (list-ref x 2)) "" `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (empty? (list-ref x 3)) "" (list-ref x 3)))
	 (jj (if (empty? (list-ref x 4)) "" `(concat ", " ,(list-ref x 4)))))
    `(concat ,ff ,vv ,ll ,jj)))

(define (format-names-rec n lim a)
  (if (equal? n lim)
      ""
      `(concat ", "
	       ,(format-name (list-ref a n))
	       ,(format-names-rec (+ n 1) lim a))))

(tm-define (format-names a)
  (:mode bib-plain?)
  (if (empty? a)
      ""
      (let* ((n (length a)))
	(if (equal? n 2)
	    (format-name (list-ref a 1))
	    (let* ((b (format-name (list-ref a 1)))
		   (m (format-names-rec 2 (- n 1) a))
		   (e (if (equal? (list-ref (list-ref a (- n 1)) 4) "others")
			  `(concat " et" (nbsp) "al")
			  `(concat ,(bib-translate " et ")
				   ,(format-name (list-ref a (- n 1)))))))
	      `(concat ,b ,m ,e))))))

(tm-define (format-author x)
  (:mode bib-plain?)
  (with a (bib-field x "author")
    (if (empty? a)
	""
	(format-names a))))

(tm-define (format-editor x)
  (:mode bib-plain?)
  (with a (bib-field x "editor")
    (if (empty? a)
	""
	(if (equal? (length a) 2)
	    `(concat ,(format-names a) ,(bib-translate ", editor"))
	    `(concat ,(format-names a) ,(bib-translate ", editors"))))))

(tm-define (format-in-ed-booktitle x)
  (:mode bib-plain?)
  (let* ((b (bib-field x "booktitle"))
	 (e (bib-field x "editor")))
    (if (empty? b)
	""
	(if (empty? e)
	    `(concat ,(bib-translate "in ") (with "font-shape" "italic" ,b))
	    `(concat ,(bib-translate "in ") ,(format-editor x) ", "
		     (with "font-shape" "italic" ,b))))))

(tm-define (format-bvolume x)
  (:mode bib-plain?)
  (let* ((v (bib-field x "volume"))
	 (s (bib-field x "series")))
    (if (empty? v)
	""
	(let ((series (if (empty? s) ""
			  `(concat ,(bib-translate " of ")
				   (with "font-shape" "italic" ,s))))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat ,(bib-translate "volume") ,sep ,v ,series)))))

(tm-define (format-number-series x)
  (:mode bib-plain?)
  (let* ((v (bib-field x "volume"))
	 (n (bib-field x "number"))
	 (s (bib-field x "series")))
    (if (empty? v)
	(if (empty? n)
	    (if (empty? s) "" s)
	    (let ((series (if (empty? s) ""
			      `(concat ,(bib-translate " in ") ,s)))
		  (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
	      `(concat ,(bib-translate "number") ,sep ,n ,series)))
	"")))

(tm-define (format-pages x)
  (:mode bib-plain?)
  (with p (bib-field x "pages")
    (cond
      ((equal? 1 (length p)) "")
      ((equal? 2 (length p))
       `(concat ,(bib-translate "page ") ,(list-ref p 1)))
      (else
	`(concat ,(bib-translate "pages ")
		 ,(list-ref p 1) "--" ,(list-ref p 2))))))

(tm-define (format-chapter-pages x)
  (:mode bib-plain?)
  (let* ((c (bib-field x "chapter"))
	 (t (bib-field x "type")))
    (if (empty? c)
	(format-pages x)
	(let ((type (if (empty? t) ,(bib-translate "chapter") (bib-locase t)))
	      (pages `(concat ", " ,(format-pages x))))
	  `(concat ,type " " ,c ,pages)))))

(tm-define (format-vol-num-pages x)
  (:mode bib-plain?)
  (let* ((v (bib-field x "volume"))
	 (n (bib-field x "number"))
	 (p (bib-field x "pages"))
	 (vol (if (empty? v) "" v))
	 (num (if (empty? n) "" `(concat "(" ,n ")")))
	 (pag (if (empty? p)
		  ""
		  (cond
		    ((equal? 1 (length p)) "")
		    ((equal? 2 (length p)) `(concat ":" ,(list-ref p 1)))
		    (else
		      `(concat ":" ,(list-ref p 1) "--" ,(list-ref p 2)))))))
    `(concat ,vol ,num ,pag)))

(tm-define (format-date x)
  (:mode bib-plain?)
  (let* ((y (bib-field x "year"))
	 (m (bib-field x "month")))
    (if (empty? y)
	(if (empty? m) "" m)
	(if (empty? m) y `(concat ,m " " ,y)))))

(tm-define (format-tr-number x)
  (:mode bib-plain?)
  (let* ((t (bib-field x "type"))
	 (n (bib-field x "number"))
	 (type (if (empty? t) ,(bib-translate "Technical Report") t))
	 (number (if (empty? n) "" n))
	 (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
    `(concat ,type ,sep ,number)))

(tm-define (format-bibitem n x)
  (:mode bib-plain?)
  `(bibitem* ,(number->string n)))

(tm-define (format-article n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (if (bib-empty? x "crossref")
	       (new-sentence
		`(,(emphasize (format-field x "journal"))
		  ,(format-vol-num-pages x)
		  ,(format-date x)))
	       (new-sentence
		`((concat ,(bib-translate "in ")
			  (cite ,(bib-field x "crossref")))
		  ,(format-pages x)))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-book n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block
	   (if (bib-empty? x "author")
	       (format-editor x)
	       (format-author x)))
	 ,(new-block
	   (new-sentence
	    `(,(emphasize (format-field x "title"))
	      ,(format-bvolume x))))
	 ,(new-block
	   (if (bib-empty? x "crossref")
	       (new-list-spc
		`(,(new-sentence
		    `(,(format-number-series x)))
		  ,(new-sentence
		    `(,(format-field x "publisher")
		      ,(format-field x "address")
		      ,(if (bib-empty? x "edition") ""
			   `(concat ,(format-field x "edition")
				    ,(bib-translate " edition")))
		      ,(format-date x)))))
	       (new-sentence
		`((concat ,(bib-translate "in ")
			  (cite ,(bib-field x "crossref")))
		  ,(format-field x "edition")
		  ,(format-date x)))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-booklet n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "howpublished")
	      ,(format-field x "address")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-inbook n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (if (bib-empty? x "author") (format-editor x)
			 (format-author x)))
	 ,(new-block
	   (new-sentence
	    `(,(emphasize (format-field x "title"))
	      ,(format-bvolume x)
	      ,(format-chapter-pages x))))
	 ,(new-block
	   (if (bib-empty? x "crossref")
	       (new-list-spc
		`(,(new-sentence `(,(format-number-series x)))
		  ,(new-sentence
		    `(,(format-field x "publisher")
		      ,(format-field x "address")
		      ,(if (bib-empty? x "edition") ""
			   `(concat ,(format-field x "edition")
				    ,(bib-translate " edition")))
		      ,(format-date x)))))
	       (new-sentence
		`(,(format-chapter-pages x)
		  (concat ,(bib-translate "in ")
			  (cite ,(bib-field x "crossref")))))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-incollection n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (if (bib-empty? x "crossref")
	       (new-list-spc
		`(,(new-sentence
		    `(,(format-in-ed-booktitle x)
		      ,(format-bvolume x)
		      ,(format-number-series x)
		      ,(format-chapter-pages x)))
		  ,(new-sentence
		    `(,(format-field x "publisher")
		      ,(format-field x "address")
		      ,(format-date x)))))
	       (new-sentence
		`((concat ,(bib-translate "in ")
			  (cite ,(bib-field x "crossref")))
		  ,(format-chapter-pages x)))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-inproceedings n x)
  (:mode bib-plain?)
  `(concat
    ,(format-bibitem n x)
    (label ,(string-append "bib-" (list-ref x 2)))
    ,(new-list-spc
      `(,(new-block (format-author x))
	,(new-block (format-field-Locase x "title"))
	,(new-block
	  (if (bib-empty? x "crossref")
	      (new-list-spc
	       `(,(new-sentence
		   `(,(format-in-ed-booktitle x)
		     ,(format-bvolume x)
		     ,(format-number-series x)
		     ,(format-pages x)))
		 ,(if (bib-empty? x "address")
		      (new-sentence
		       `(,(format-field x "organization")
			 ,(format-field x "publisher")
			 ,(format-date x)))
		      (new-list-spc
		       `(,(new-sentence
			   `(,(format-field x "address")
			     ,(format-date x)))
			 ,(new-sentence
			   `(,(format-field x "organization")
			     ,(format-field x "publisher"))))))))
	      (new-sentence
	       `((concat ,(bib-translate "in ")
			 (cite ,(bib-field x "crossref")))
		 ,(format-pages x)))))
	,(new-block (format-field x "note"))))))

(tm-define (format-manual n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block
	   (if (bib-empty? x "author")
	       (if (bib-empty? x "organization") ""
		   (new-sentence
		    `(,(format-field x "organization")
		      ,(format-field x "address"))))
	       (format-author x)))
	 ,(new-block (emphasize (format-field x "title")))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "organization")
	      ,(format-field x "address")
	      ,(if (bib-empty? x "edition") ""
		   `(concat ,(format-field x "edition")
			    ,(bib-translate " edition")))
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-mastersthesis n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (new-sentence
	    `(,(if (bib-empty? x "type")
		   ,(bib-translate "Master's thesis")
		   (format-field-Locase x "type"))
	      ,(format-field x "school")
	      ,(format-field x "address")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-misc n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "howpublished")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-phdthesis n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (emphasize (format-field x "title")))
	 ,(new-block
	   (new-sentence
	    `(,(if (bib-empty? x "type")
		   (bib-translate "PhD thesis")
		   (format-field-Locase x "type"))
	      ,(format-field x "school")
	      ,(format-field x "address")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-proceedings n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block
	   (if (bib-empty? x "editor")
	       (format-field x "organization")
	       (format-editor x)))
	 ,(new-block
	   (new-sentence
	    `(,(emphasize (format-field x "title"))
	      ,(format-bvolume x)
	      ,(format-number-series x))))
	 ,(new-block
	   (if (bib-empty? x "address")
	       (new-sentence
		`(,(if (bib-empty? x "editor") ""
		       (format-field x "organization"))
		  ,(format-field x "publisher")
		  ,(format-date x)))
	       (new-list-spc
		`(,(new-sentence
		    `(,(format-field x "address")
		      ,(format-date x)))
		  ,(new-sentence
		    `(,(if (bib-empty? x "editor") ""
			   (format-field x "organization"))
		      ,(format-field x "publisher")))))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-techreport n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (new-sentence
	    `(,(format-tr-number x)
	      ,(format-field x "institution")
	      ,(format-field x "address")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-unpublished n x)
  (:mode bib-plain?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list-spc
       `(,(new-block (format-author x))
	 ,(new-block (format-field-Locase x "title"))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "note")
	      ,(format-date x))))))))

(tm-define (format-entry n x)
  (:mode bib-plain?)
  (if (and (list? x) (func? x 'bib-entry)
	   (= (length x) 4) (func? (list-ref x 3) 'document))
      (with doctype (list-ref x 1)
	(cond
	  ((equal? doctype "article") (format-article n x))
	  ((equal? doctype "book") (format-book n x))
	  ((equal? doctype "booklet") (format-booklet n x))
	  ((equal? doctype "inbook") (format-inbook n x))
	  ((equal? doctype "incollection") (format-incollection n x))
	  ((equal? doctype "inproceedings") (format-inproceedings n x))
	  ((equal? doctype "conference") (format-inproceedings n x))
	  ((equal? doctype "manual") (format-manual n x))
	  ((equal? doctype "mastersthesis") (format-mastersthesis n x))
	  ((equal? doctype "misc") (format-misc n x))
	  ((equal? doctype "phdthesis") (format-phdthesis n x))
	  ((equal? doctype "proceedings") (format-proceedings n x))
	  ((equal? doctype "techreport") (format-techreport n x))
	  ((equal? doctype "unpublished") (format-unpublished n x))
	  (else (format-misc n x))))))

(define (author-sort-format a)
  (if (null? a)
      ""
      (with name
	  (let* ((x (car a))
		 (ff (if (equal? (list-ref x 1) "") ""
			 (string-append (bib-purify (list-ref x 1)) " ")))
		 (vv (if (equal? (list-ref x 2) "") ""
			 (string-append (bib-purify (list-ref x 2)) " ")))
		 (ll (if (equal? (list-ref x 3) "") ""
			 (string-append (bib-purify (list-ref x 3)) " ")))
		 (jj (if (equal? (list-ref x 4) "") ""
			 (string-append (bib-purify (list-ref x 4)) " "))))
	    (string-append vv ll ff jj))
	(string-append name (author-sort-format (cdr a))))))

(define (author-editor-sort-key x)
  (if (bib-empty? x "author")
      (if (bib-empty? x "editor")
	  (list-ref x 2)
	  (author-sort-format (cdr (bib-field x "editor"))))
      (author-sort-format (cdr (bib-field x "author")))))

(define (author-sort-key x ae)
  (if (bib-empty? x ae)
      (list-ref x 2)
      (author-sort-format (cdr (bib-field x ae)))))

(tm-define (bib-sort-key x)
  (:mode bib-plain?)
  (let* ((doctype (list-ref x 1))
	 (pre (cond
		((or (equal? doctype "inbook") (equal? doctype "book"))
		 (author-editor-sort-key x))
		((equal? doctype "proceedings")
		 (author-sort-key x "editor"))
		(else
		  (author-sort-key x "author")))))
    (string-append pre "    "
		   (bib-field x "year") "    "
		   (bib-purify (bib-field x "title")))))

