
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : acm.scm
;; DESCRIPTION : acm style for BibTeX files
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex acm)
  (:use (bibtex bib-utils)))

(bib-define-style "acm" "plain")

(tm-define (format-name x)
  (:mode bib-acm?)
  (let* ((f (if (empty? (list-ref x 1))
		""
		`(concat ", " ,(bib-abbreviate (list-ref x 1) "." `(nbsp)))))
	 (vv (if (empty? (list-ref x 2)) "" `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (empty? (list-ref x 3)) "" (list-ref x 3)))
	 (jj (if (empty? (list-ref x 4)) "" `(concat ", " ,(list-ref x 4)))))
    `(with "font-shape" "small-caps" (concat ,vv ,ll ,jj ,f))))

(tm-define (format-editor x)
  (:mode bib-acm?)
  (let* ((a (bib-field x "editor")))
    (if (empty? a)
	""
	(if (equal? (length a) 2)
	    `(concat ,(format-names a) ,(bib-translate ", Ed."))
	    `(concat ,(format-names a) ,(bib-translate ", Eds."))))))

(tm-define (format-date x)
  (:mode bib-acm?)
  (let* ((y (bib-field x "year"))
	 (m (bib-field x "month")))
    (if (empty? y)
	(if (empty? m) "" m)
	(if (empty? m) y `(concat ,m " " ,y)))))

(tm-define (format-in-ed-booktitle x)
  (:mode bib-acm?)
  (let* ((b (bib-field x "booktitle"))
	 (a (bib-field x "address"))
	 (cl `(concat " (" ,(new-list ", " `(,a ,(format-date x))) ")")))
    (if (empty? b)
	""
	`(concat
	  ,(bib-translate "in ")
	  (with "font-shape" "italic" ,b) ,cl))))

(tm-define (format-volume-or-number x)
  (:mode bib-acm?)
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
	      `(concat "no." ,sep ,n ,series)))
	(let ((series (if (empty? s)
			  ""
			  `(concat ,(bib-translate " of ")
				   (with "font-shape" "italic" ,s))))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat "vol." ,sep ,v ,series)))))

(tm-define (format-pages x)
  (:mode bib-acm?)
  (let* ((p (bib-field x "pages")))
    (cond
     ((equal? 1 (length p)) "")
     ((equal? 2 (length p)) `(concat ,(bib-translate "p. ") ,(list-ref p 1)))
     (else `(concat ,(bib-translate "p. ")
		    ,(list-ref p 1) "--" ,(list-ref p 2))))))

(tm-define (format-chapter-pages x)
  (:mode bib-acm?)
  (let* ((c (bib-field x "chapter"))
	 (t (bib-field x "type")))
    (if (empty? c)
	(format-pages x)
	(let ((type (if (empty? t)
			,(bib-translate "chapter")
			(bib-locase t)))
	      (pages `(concat ", " ,(format-pages x))))
	  `(concat ,type " " ,c ,pages)))))

(tm-define (format-tr-number x)
  (:mode bib-acm?)
  (let* ((t (bib-field x "type"))
	 (n (bib-field x "number"))
	 (type (if (empty? t) ,(bib-translate "Technical Report") t))
	 (number (if (empty? n) "" n))
	 (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
    `(concat ,type ,sep ,number)))

(tm-define (format-edition x)
  (:mode bib-acm?)
  (let* ((e (bib-field x "edition")))
    (if (empty? e) "" `(concat ,e " ed."))))

(tm-define (format-bibitem n x)
  (:mode bib-acm?)
  `(bibitem* ,(number->string n)))

(tm-define (format-article n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block (format-author x))
	       ,(new-block (format-field-Locase x "title"))
	       ,(new-block
		 (if (bib-empty? x "crossref")
		     (new-sentence
		      `(,(emphasize `(concat ,(format-field x "journal")
					     " "
					     ,(format-field x "volume")))
			(concat ,(format-field x "number")
				" ("
				,(format-date x) ")") 
			,(format-pages x)))
		     (new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-pages x)))))
	       ,(new-block (format-field x "note"))))))

(tm-define (format-book n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block (if (bib-empty? x "author")
			       (format-editor x)
			       (format-author x)))
	       ,(new-block
		 (new-sentence
		  `(,(emphasize (format-field x "title"))
		    ,(format-edition x))))
	       ,(new-block
		 (if (bib-empty? x "crossref")
		     (new-list-spc
		      `(,(new-sentence `(,(format-number-series x)))
			,(new-sentence
			  `(,(format-field x "publisher")
			    ,(format-field x "address")
			    ,(format-date x)))))
		     (new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-field x "edition")
			,(format-date x)))))
	       ,(new-block (format-field x "note"))))))

(tm-define (format-inbook n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block (if (bib-empty? x "author")
			       (format-editor x)
			       (format-author x)))
	       ,(new-block
		 (new-sentence
		  `(,(emphasize (format-field x "title")))))
	       ,(new-block
		 (if (bib-empty? x "crossref")
		     (new-list-spc
		      `(,(new-sentence `(,(format-number-series x)))
			,(new-sentence
			  `(,(format-field x "publisher")
			    ,(format-field x "address")
			    ,(format-date x)
			    ,(format-chapter-pages x)))))
		     (new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-field x "edition")
			,(format-date x)))))
	       ,(new-block (format-field x "note"))))))

(tm-define (format-incollection n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block (format-author x))
	       ,(new-block (format-field-Locase x "title"))
	       ,(new-block
		 (if (bib-empty? x "crossref")
		     (new-list-spc
		      `(,(new-sentence
			  `((concat ,(bib-translate "in ")
				    ,(emphasize (format-field x "booktitle")))
			    ,(format-editor x)
			    ,(format-edition x)
			    ,(format-volume-or-number x)))
			,(new-sentence
			  `(,(format-field x "publisher")
			    ,(format-field x "address")
			    ,(format-date x)
			    ,(format-chapter-pages x)))))
		     (new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-chapter-pages x)))))
	       ,(new-block (format-field x "note"))))))

(tm-define (format-inproceedings n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block (format-author x))
	       ,(new-block (format-field-Locase x "title"))
	       ,(new-block
		 (if (bib-empty? x "crossref")
		     (new-list-spc
		      `(,(new-sentence
			  `(,(format-in-ed-booktitle x)
			    ,(format-editor x)
			    ,(format-volume-or-number x)
			    ,(format-field x "organization")
			    ,(format-field x "publisher")
			    ,(format-pages x)))))
		     (new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(format-pages x)))))
	       ,(new-block (format-field x "note"))))))

(tm-define (format-manual n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block (format-author x))
	       ,(new-block
		 (new-sentence
		  `(,(emphasize (format-field x "title"))
		    ,(format-edition x)
		    ,(format-field x "organization")
		    ,(format-field x "address")
		    ,(format-date x))))
	       ,(new-block (format-field x "note"))))))

(tm-define (format-proceedings n x)
  (:mode bib-acm?)
  `(concat ,(format-bibitem n x)
	   (label ,(string-append "bib-" (list-ref x 2)))
	   ,(new-list-spc
	     `(,(new-block
		 (if (bib-empty? x "editor")
		     (format-field x "organization")
		     (format-editor x)))
	       ,(new-block
		 (new-sentence
		  `((concat ,(emphasize (format-field x "title"))
			    " ("
			    ,(new-list ", "
				       `(,(format-field x "address")
					 ,(format-date x)))
			    ")")
		    ,(format-volume-or-number x)
		    ,(format-field x "organization")
		    ,(format-field x "publisher"))))
	       ,(new-block (format-field x "note"))))))

