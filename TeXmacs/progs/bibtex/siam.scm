
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : siam.scm
;; DESCRIPTION : siam style for BibTeX files
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex siam)
  (:use (bibtex bib-utils)))

(bib-define-style "siam" "plain")

(tm-define (format-name x)
  (:mode bib-siam?)
  (let* ((f (if (empty? (list-ref x 1))
		""
		`(concat ,(bib-abbreviate (list-ref x 1) "." `(nbsp)) (nbsp))))
	 (vv (if (empty? (list-ref x 2)) "" `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (empty? (list-ref x 3)) "" (list-ref x 3)))
	 (jj (if (empty? (list-ref x 4)) "" `(concat ", " ,(list-ref x 4)))))
    `(with "font-shape" "small-caps" (concat ,f ,vv ,ll ,jj))))

(tm-define (format-editor x)
  (:mode bib-siam?)
  (with a (bib-field x "editor")
    (if (empty? a) ""
	(if (equal? (length a) 2)
	    `(concat ,(format-names a) ", ed.")
	    `(concat ,(format-names a) ", eds.")))))

(tm-define (format-bvolume x)
  (:mode bib-siam?)
  (let* ((v (bib-field x "volume"))
	 (s (bib-field x "series")))
    (if (empty? v) ""
	(let ((series (if (empty? s) ""
			  `(concat ,(bib-translate " of ") ,s)))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat "vol." ,sep ,v ,series)))))

(tm-define (format-number-series x)
  (:mode bib-siam?)
  (let* ((v (bib-field x "volume"))
	 (n (bib-field x "number"))
	 (s (bib-field x "series")))
    (if (empty? v)
	(if (empty? n)
	    (if (empty? s) "" s)
	    (let ((series (if (empty? s) ""
			      `(concat ,(bib-translate " in ") ,s)))
		  (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
	      `(concat "no." ,sep ,n ,series)))
	"")))

(tm-define (format-edition x)
  (:mode bib-siam?)
  (with e (bib-field x "edition")
    (if (empty? e)
	""
	`(concat ,e " ed."))))

(tm-define (format-in-ed-booktitle x)
  (:mode bib-siam?)
  (let* ((b (bib-field x "booktitle"))
	 (e (bib-field x "editor")))
    (if (empty? b) ""
	(if (empty? e)
	    `(concat ,(bib-translate "in ") ,b)
	    `(concat ,(bib-translate "in ") ,b ", " ,(format-editor x))))))

(tm-define (format-chapter-pages x)
  (:mode bib-siam?)
  (let* ((c (bib-field x "chapter"))
	 (t (bib-field x "type")))
    (if (empty? c)
	(format-pages x)
	(let ((type (if (empty? t) ,(bib-translate "chapter") (bib-locase t)))
	      (pages `(concat ", " ,(format-pages x))))
	  `(concat ,type " " ,c ,pages)))))

(tm-define (format-pages x)
  (:mode bib-siam?)
  (with p (bib-field x "pages")
    (cond
      ((equal? 1 (length p)) "")
      ((equal? 2 (length p)) `(concat "p. " ,(list-ref p 1)))
      (else `(concat "p. " ,(list-ref p 1) "--" ,(list-ref p 2))))))

(tm-define (format-article n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field-Locase x "title"))
	      ,@(if (bib-empty? x "crossref")
		    `(,(format-field x "journal")
		      (concat
			,(bib-field x "volume")
			,(if (bib-empty? x "year") ""
			     `(concat
				,(if (bib-empty? x "volume") "" " ")
				"(" ,(bib-field x "year") ")")))
		      ,(format-pages x))
		    `((concat ,(bib-translate "in ")
			      (cite ,(bib-field x "crossref")))
		      ,(format-pages x))))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-book n x)
  (:mode bib-siam?)
  `(concat
    ,(format-bibitem n x)
    (label ,(string-append "bib-" (list-ref x 2)))
    ,(new-list
      " "
      `(,(new-block
	  (new-sentence
	   `(,(if (bib-empty? x "author") (format-editor x) (format-author x))
	     ,(emphasize (format-field x "title"))
	     ,@(if (bib-empty? x "crossref")
		   `(,(format-bvolume x)
		     ,(format-number-series x)
		     ,(format-field x "publisher")
		     ,(format-field x "address")
		     ,(format-edition x)
		     ,(format-date x))
		   `((concat ,(bib-translate "in ")
			     (cite ,(bib-field x "crossref")))
		     ,(format-field x "edition")
		     ,(format-date x))))))
	,(new-block (format-field x "note"))))))

(tm-define (format-booklet n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field-Locase x "title")))))
	 ,(new-block
	   (new-sentence `(,(format-field x "howpublished")
			   ,(format-field x "address")
			   ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-inbook n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(if (bib-empty? x "author") (format-editor x) (format-author x))
	      ,(emphasize (format-field x "title"))
	      ,@(if (bib-empty? x "crossref")
		    `(,(format-bvolume x)
		      ,(format-number-series x)
		      ,(format-field x "publisher")
		      ,(format-field x "address")
		      ,(format-edition x)
		      ,(format-date x)
		      ,(format-chapter-pages x))
		    `((concat ,(bib-translate "in ")
			      (cite ,(bib-field x "crossref")))
		      ,(format-date x))))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-incollection n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field-Locase x "title"))
	      ,@(if (bib-empty? x "crossref")
		    `(,(format-in-ed-booktitle x)
		      ,(format-bvolume x)
		      ,(format-number-series x)
		      ,(format-field x "publisher")
		      ,(format-field x "address")
		      ,(format-edition x)
		      ,(format-date x)
		      ,(format-chapter-pages x))
		    `((concat ,(bib-translate "in ")
			      (cite ,(bib-field x "crossref")))
		      ,(format-chapter-pages x))))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-inproceedings n x)
  (:mode bib-siam?)
  `(concat
    ,(format-bibitem n x)
    (label ,(string-append "bib-" (list-ref x 2)))
    ,(new-list
      " "
      `(,(new-block
	  (new-sentence
	   `(,(format-author x)
	     ,(emphasize (format-field-Locase x "title"))
	     ,@(if (bib-empty? x "crossref")
		   `(,(format-in-ed-booktitle x)
		     ,(format-bvolume x)
		     ,(format-number-series x)
		     ,(format-field x "address")
		     ,(format-date x)
		     ,(format-field x "organization")
		     ,(format-field x "publisher")
		     ,(format-pages x))
		   `((concat ,(bib-translate "in ")
			     (cite ,(bib-field x "crossref")))
		     ,(format-pages x))))))
	,(new-block (format-field x "note"))))))

(tm-define (format-manual n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,@(if (bib-empty? x "author")
		    (if (bib-empty? x "organization")
			`()
			`(,(format-field x "organization")
			  ,(format-field x "address")))
		    `(,(format-author x)))
	      ,(emphasize (format-field x "title")))))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "organization")
	      ,(format-field x "address")
	      ,(format-edition x)
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-mastersthesis n x)
  (:mode bib-siam?)
  `(concat
    ,(format-bibitem n x)
    (label ,(string-append "bib-" (list-ref x 2)))
    ,(new-list
      " "
      `(,(new-block
	  (new-sentence
	   `(,(format-author x)
	     ,(emphasize (format-field-Locase x "title"))
	     ,(if (bib-empty? x "type")
		  (bib-translate "Master's thesis")
		  (format-field x "type"))
	     ,(format-field x "school")
	     ,(format-field x "address")
	     ,(format-date x))))
	,(new-block (format-field x "note"))))))

(tm-define (format-misc n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field-Locase x "title")))))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "howpublished")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-phdthesis n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field x "title"))
	      ,(if (bib-empty? x "type")
		   (bib-translate "Master's thesis")
		   (format-field x "type"))
	      ,(format-field x "school")
	      ,(format-field x "address")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-proceedings n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(if (bib-empty? x "editor")
		   (format-field x "organization")
		   (format-editor x))
	      ,(emphasize (format-field x "title"))
	      ,(format-bvolume x)
	      ,(format-number-series x)
	      ,(format-field x "address")
	      ,(format-date x)
	      ,(format-field x "organization")
	      ,(format-field x "publisher"))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-techreport n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field-Locase x "title"))
	      ,(format-tr-number x)
	      ,(format-field x "institution")
	      ,(format-field x "address")
	      ,(format-date x))))
	 ,(new-block (format-field x "note"))))))

(tm-define (format-unpublished n x)
  (:mode bib-siam?)
  `(concat
     ,(format-bibitem n x)
     (label ,(string-append "bib-" (list-ref x 2)))
     ,(new-list
       " "
       `(,(new-block
	   (new-sentence
	    `(,(format-author x)
	      ,(emphasize (format-field-Locase x "title")))))
	 ,(new-block
	   (new-sentence
	    `(,(format-field x "note")
	      ,(format-date x))))))))
