
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : elsart-num.scm
;; DESCRIPTION : elsart-num style for BibTeX files
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex elsart-num)
  (:use (bibtex bib-utils)))

(bib-define-style "elsart-num" "plain")

(tm-define (format-name x)
  (:mode bib-elsart-num?)
  (let* ((f (if (empty? (list-ref x 1))
		""
		`(concat ,(bib-abbreviate (list-ref x 1) "." `(nbsp)))))
	 (vv (if (empty? (list-ref x 2)) "" `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (empty? (list-ref x 3)) "" (list-ref x 3)))
	 (jj (if (empty? (list-ref x 4)) "" `(concat ", " ,(list-ref x 4)))))
    `(concat ,f ,vv ,ll ,jj)))

(tm-define (format-editor x)
  (:mode bib-elsart-num?)
  (let* ((a (bib-field x "editor")))
    (if (empty? a)
	""
	(if (equal? (length a) 2)
	    `(concat ,(format-names a) ,(bib-translate " (Ed.)"))
	    `(concat ,(format-names a) ,(bib-translate " (Eds.)"))))))

(tm-define (format-edition x)
  (:mode bib-elsart-num?)
  (let* ((e (bib-field x "edition")))
    (if (empty? e) "" `(concat ,e " Edition"))))

(tm-define (format-volume-or-number x)
  (:mode bib-elsart-num?)
  (let* ((v (bib-field x "volume"))
	 (n (bib-field x "number"))
	 (s (bib-field x "series")))
    (if (empty? v)
	(if (empty? n)
	    (if (empty? s) "" s)
	    (let ((series (if (empty? s)
			      ""
			      `(concat ,(bib-translate " in ") ,s)))
		  (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
	      `(concat "No." ,sep ,n ,series)))
	(let ((series (if (empty? s)
			  ""
			  `(concat ,(bib-translate " of ") ,s)))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat "Vol." ,sep ,v ,series)))))

(tm-define (format-pages x)
  (:mode bib-elsart-num?)
  (let* ((p (bib-field x "pages")))
    (cond
     ((equal? 1 (length p)) "")
     ((equal? 2 (length p)) `(concat ,(bib-translate "pp. ")
				     ,(list-ref p 1)))
     (else `(concat ,(bib-translate "pp. ")
		    ,(list-ref p 1) "--" ,(list-ref p 2))))))

(tm-define (format-vol-num-pages x)
  (:mode bib-elsart-num?)
  (let* ((j (bib-field x "journal"))
	 (v (bib-field x "volume"))
	 (n `(concat "(" ,(bib-field x "number") ")"))
	 (y `(concat "(" ,(bib-field x "year") ")"))
	 (p (let* ((pp (bib-field x "pages")))
	      (cond
	       ((equal? 1 (length pp)) "")
	       ((equal? 2 (length pp)) (list-ref pp 1))
	       (else `(concat ,(list-ref pp 1) "--" ,(list-ref pp 2)))))))
    (new-list " " `(,j ,v ,n ,y ,p))))

(tm-define (format-article n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		,@(if (bib-empty? x "crossref")
		      `(,(format-vol-num-pages x))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-pages x)))
		,(format-field x "note"))))))

(tm-define (format-book n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(if (bib-empty? x "author")
		     (format-editor x)
		     (format-author x))
		,(format-field x "title")
		,(format-edition x)
		,@(if (bib-empty? x "crossref")
		      `(,(format-volume-or-number x)
			,(format-field x "publisher")
			,(format-field x "address"))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-edition x )))
		,(format-field x "year")
		,(format-field x "note"))))))

(tm-define (format-booklet n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		,(format-field x "howpublished")
		,(format-field x "address")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(format-field x "note") " "))
			"(" ,(format-date x) ")"))))))

(tm-define (format-inbook n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(if (bib-empty? x "author")
		     (format-editor x)
		     (format-author x))
		,(format-field x "title")
		,@(if (bib-empty? x "crossref")
		      `(,(format-edition x)
			,(format-volume-or-number x)
			,(format-field x "publisher")
			,(format-field x "address")
			,(format-field x "year")
			,(format-chapter-pages x))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-field x "edition")
			,(format-field x "year")))
		,(format-field x "note"))))))

(tm-define (format-incollection n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		,@(if (bib-empty? x "crossref")
		      `((concat ,(bib-translate "in: ")
				,(format-editor x))
			,(format-field x "booktitle")
			,(format-edition x)
			,(format-volume-or-number x)
			,(format-field x "publisher")
			,(format-field x "address")
			,(format-field x "year"))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))))
		,(format-chapter-pages x)
		,(format-field x "note"))))))

(tm-define (format-inproceedings n x)
  (:mode bib-elsart-num?)
  (format-incollection n x))

(tm-define (format-manual n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,@(if (bib-empty? x "author")
		      (if (bib-empty? x "organization")
			  `()
			  `(,(format-field x "organization")
			    ,(format-field x "address")))
		      `(,(format-author x)))
		,(format-field x "title")
		,(format-field x "organization")
		,(format-field x "address")
		,(format-edition x)
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(format-field x "note") " "))
			"(" ,(format-date x) ")"))))))

(tm-define (format-mastersthesis n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		,(if (bib-empty? x "type")
		     ,(bib-translate "Master's thesis")
		     (format-field x "type"))
		,(format-field x "school")
		,(format-field x "address")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(format-field x "note") " "))
			"(" ,(format-date x) ")"))))))

(tm-define (format-misc n x)
  (:mode bib-plain?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		,(format-field x "howpublished")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(format-field x "note") " "))
			"(" ,(format-date x) ")"))))))

(tm-define (format-phdthesis n x)
  (:mode bib-elsart-num?)
  (format-mastersthesis n x))

(tm-define (format-proceedings n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(if (bib-empty? x "editor")
		     (format-field x "organization")
		     (format-editor x))
		,(format-field x "title")
		,(format-volume-or-number x)
		,(if (bib-empty? x "editor")
		     ""
		     (format-field x "organization"))
		,(format-field x "publisher")
		,(format-field x "address")
		,(format-field x "year")
		,(format-field x "note"))))))

(tm-define (format-techreport n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		,(format-tr-number x)
		,(format-field x "institution")
		,(format-field x "address")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(format-field x "note") " "))
			"(" ,(format-date x) ")"))))))

(tm-define (format-unpublished n x)
  (:mode bib-elsart-num?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-block
	     (new-sentence
	      `(,(format-author x)
		,(format-field-Locase x "title")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(format-field x "note") " "))
			"(" ,(format-date x) ")"))))))

