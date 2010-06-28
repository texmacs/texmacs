
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

(tm-define (bib-format-name x)
  (:mode bib-elsart-num?)
  (let* ((f (if (bib-null? (list-ref x 1))
		""
		`(concat ,(bib-abbreviate (list-ref x 1) "." `(nbsp)))))
	 (vv (if (bib-null? (list-ref x 2)) "" `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (bib-null? (list-ref x 3)) "" (list-ref x 3)))
	 (jj (if (bib-null? (list-ref x 4)) "" `(concat ", " ,(list-ref x 4)))))
    `(concat ,f ,vv ,ll ,jj)))

(tm-define (bib-format-editor x)
  (:mode bib-elsart-num?)
  (let* ((a (bib-field x "editor")))
    (if (bib-null? a)
	""
	(if (equal? (length a) 2)
	    `(concat ,(bib-format-names a) ,(bib-translate " (Ed.)"))
	    `(concat ,(bib-format-names a) ,(bib-translate " (Eds.)"))))))

(tm-define (bib-format-edition x)
  (:mode bib-elsart-num?)
  (let* ((e (bib-field x "edition")))
    (if (bib-null? e) "" `(concat ,e " Edition"))))

(tm-define (bib-format-volume-or-number x)
  (:mode bib-elsart-num?)
  (let* ((v (bib-field x "volume"))
	 (n (bib-field x "number"))
	 (s (bib-field x "series")))
    (if (bib-null? v)
	(if (bib-null? n)
	    (if (bib-null? s) "" s)
	    (let ((series (if (bib-null? s)
			      ""
			      `(concat ,(bib-translate " in ") ,s)))
		  (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
	      `(concat "No." ,sep ,n ,series)))
	(let ((series (if (bib-null? s)
			  ""
			  `(concat ,(bib-translate " of ") ,s)))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat "Vol." ,sep ,v ,series)))))

(tm-define (bib-format-pages x)
  (:mode bib-elsart-num?)
  (let* ((p (bib-field x "pages")))
    (cond
     ((equal? 1 (length p)) "")
     ((equal? 2 (length p)) `(concat ,(bib-translate "pp. ")
				     ,(list-ref p 1)))
     (else `(concat ,(bib-translate "pp. ")
		    ,(list-ref p 1) "--" ,(list-ref p 2))))))

(tm-define (bib-format-vol-num-pages x)
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
    (bib-new-list " " `(,j ,v ,n ,y ,p))))

(tm-define (bib-format-article n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		,@(if (bib-empty? x "crossref")
		      `(,(bib-format-vol-num-pages x))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-pages x)))
		,(bib-format-field x "note"))))))

(tm-define (bib-format-book n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(if (bib-empty? x "author")
		     (bib-format-editor x)
		     (bib-format-author x))
		,(bib-format-field x "title")
		,(bib-format-edition x)
		,@(if (bib-empty? x "crossref")
		      `(,(bib-format-volume-or-number x)
			,(bib-format-field x "publisher")
			,(bib-format-field x "address"))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-edition x )))
		,(bib-format-field x "year")
		,(bib-format-field x "note"))))))

(tm-define (bib-format-booklet n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		,(bib-format-field x "howpublished")
		,(bib-format-field x "address")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(bib-format-field x "note") " "))
			"(" ,(bib-format-date x) ")"))))))

(tm-define (bib-format-inbook n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(if (bib-empty? x "author")
		     (bib-format-editor x)
		     (bib-format-author x))
		,(bib-format-field x "title")
		,@(if (bib-empty? x "crossref")
		      `(,(bib-format-edition x)
			,(bib-format-volume-or-number x)
			,(bib-format-field x "publisher")
			,(bib-format-field x "address")
			,(bib-format-field x "year")
			,(bib-format-chapter-pages x))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-field x "edition")
			,(bib-format-field x "year")))
		,(bib-format-field x "note"))))))

(tm-define (bib-format-incollection n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		,@(if (bib-empty? x "crossref")
		      `((concat ,(bib-translate "in: ")
				,(bib-format-editor x))
			,(bib-format-field x "booktitle")
			,(bib-format-edition x)
			,(bib-format-volume-or-number x)
			,(bib-format-field x "publisher")
			,(bib-format-field x "address")
			,(bib-format-field x "year"))
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))))
		,(bib-format-chapter-pages x)
		,(bib-format-field x "note"))))))

(tm-define (bib-format-inproceedings n x)
  (:mode bib-elsart-num?)
  (bib-format-incollection n x))

(tm-define (bib-format-manual n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,@(if (bib-empty? x "author")
		      (if (bib-empty? x "organization")
			  `()
			  `(,(bib-format-field x "organization")
			    ,(bib-format-field x "address")))
		      `(,(bib-format-author x)))
		,(bib-format-field x "title")
		,(bib-format-field x "organization")
		,(bib-format-field x "address")
		,(bib-format-edition x)
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(bib-format-field x "note") " "))
			"(" ,(bib-format-date x) ")"))))))

(tm-define (bib-format-mastersthesis n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		,(if (bib-empty? x "type")
		     ,(bib-translate "Master's thesis")
		     (bib-format-field x "type"))
		,(bib-format-field x "school")
		,(bib-format-field x "address")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(bib-format-field x "note") " "))
			"(" ,(bib-format-date x) ")"))))))

(tm-define (bib-format-misc n x)
  (:mode bib-plain?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		,(bib-format-field x "howpublished")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(bib-format-field x "note") " "))
			"(" ,(bib-format-date x) ")"))))))

(tm-define (bib-format-phdthesis n x)
  (:mode bib-elsart-num?)
  (bib-format-mastersthesis n x))

(tm-define (bib-format-proceedings n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(if (bib-empty? x "editor")
		     (bib-format-field x "organization")
		     (bib-format-editor x))
		,(bib-format-field x "title")
		,(bib-format-volume-or-number x)
		,(if (bib-empty? x "editor")
		     ""
		     (bib-format-field x "organization"))
		,(bib-format-field x "publisher")
		,(bib-format-field x "address")
		,(bib-format-field x "year")
		,(bib-format-field x "note"))))))

(tm-define (bib-format-techreport n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		,(bib-format-tr-number x)
		,(bib-format-field x "institution")
		,(bib-format-field x "address")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(bib-format-field x "note") " "))
			"(" ,(bib-format-date x) ")"))))))

(tm-define (bib-format-unpublished n x)
  (:mode bib-elsart-num?)
  `(concat ,(bib-format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   :promptrepl
           ,(bib-new-block
	     (bib-new-sentence
	      `(,(bib-format-author x)
		,(bib-format-field-Locase x "title")
		(concat ,(if (bib-empty? x "note")
			     ""
			     `(concat ,(bib-format-field x "note") " "))
			"(" ,(bib-format-date x) ")"))))))

