
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : siam.scm
;; DESCRIPTION : siam style for BibTeX files
;; COPYRIGHT   : (C) 2010, 2015  David MICHEL, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex siam)
  (:use (bibtex bib-utils) (bibtex plain)))

(bib-define-style "siam" "plain")

(tm-define (bib-format-name x)
  (:mode bib-siam?)
  (let* ((f (if (bib-null? (list-ref x 1)) ""
		`(concat ,(bib-abbreviate (list-ref x 1) "." `(nbsp)) (nbsp))))
	 (vv (if (bib-null? (list-ref x 2)) ""
                 `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (bib-null? (list-ref x 3)) ""
                 (bib-purify (list-ref x 3))))
	 (jj (if (bib-null? (list-ref x 4)) ""
                 `(concat ", " ,(list-ref x 4)))))
    `(with "font-shape" "small-caps" (concat ,f ,vv ,ll ,jj))))

(tm-define (bib-format-editor x)
  (:mode bib-siam?)
  (with a (bib-field x "editor")
    (if (or (bib-null? a) (nlist? a)) ""
	(if (equal? (length a) 2)
	    `(concat ,(bib-format-names a) ", ed.")
	    `(concat ,(bib-format-names a) ", eds.")))))

(tm-define (bib-format-bvolume x)
  (:mode bib-siam?)
  (let* ((v (bib-field x "volume"))
	 (s (bib-field x "series")))
    (if (bib-null? v) ""
	(let ((series (if (bib-null? s) ""
			  `(concat ,(bib-translate " of ") ,s)))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat "vol." ,sep ,v ,series)))))

(tm-define (bib-format-number-series x)
  (:mode bib-siam?)
  (let* ((v (bib-field x "volume"))
	 (n (bib-field x "number"))
	 (s (bib-field x "series")))
    (if (bib-null? v)
	(if (bib-null? n)
	    (if (bib-null? s) "" s)
	    (let ((series (if (bib-null? s) ""
			      `(concat ,(bib-translate " in ") ,s)))
		  (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
	      `(concat "no." ,sep ,n ,series)))
	"")))

(define (bib-format-edition x)
  (with e (bib-field x "edition")
    (if (bib-null? e)
	""
	`(concat ,e " ed."))))

(tm-define (bib-format-in-ed-booktitle x)
  (:mode bib-siam?)
  (let* ((b (bib-field x "booktitle"))
	 (e (bib-field x "editor")))
    (if (bib-null? b) ""
	(if (bib-null? e)
	    `(concat ,(bib-translate "in ") ,b)
	    `(concat ,(bib-translate "in ") ,b ", " ,(bib-format-editor x))))))

(tm-define (bib-format-chapter-pages x)
  (:mode bib-siam?)
  (let* ((c (bib-field x "chapter"))
	 (t (bib-field x "type")))
    (if (bib-null? c)
	(bib-format-pages x)
	(let ((type (if (bib-null? t) (bib-translate "chapter") (bib-locase t)))
	      (pages `(concat ", " ,(bib-format-pages x))))
	  `(concat ,type " " ,c ,pages)))))

(tm-define (bib-format-pages x)
  (:mode bib-siam?)
  (with p (bib-field x "pages")
    (cond
      ((or (bib-null? p) (nlist? p)) "")
      ((== (length p) 1) "")
      ((== (length p) 2) `(concat "p." (nbsp) ,(list-ref p 1)))
      (else `(concat "pp." (nbsp)
                     ,(list-ref p 1) ,bib-range-symbol ,(list-ref p 2))))))

(tm-define (bib-format-article n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field-Locase x "title"))
	      ,@(if (bib-empty? x "crossref")
		    `(,(bib-format-field x "journal")
		      (concat
			,(bib-field x "volume")
			,(if (bib-empty? x "year") ""
			     `(concat
				,(if (bib-empty? x "volume") "" " ")
				"(" ,(bib-field x "year") ")")))
		      ,(bib-format-pages x))
		    `((concat ,(bib-translate "in ")
			      (cite ,(bib-field x "crossref")))
		      ,(bib-format-pages x))))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-book n x)
  (:mode bib-siam?)
  `(concat
    ,(bib-format-bibitem n x)
    ,(bib-label (list-ref x 2))
    ,(bib-new-list-spc
      `(,(bib-new-block
	  (bib-new-sentence
	   `(,(if (bib-empty? x "author") (bib-format-editor x) (bib-format-author x))
	     ,(bib-emphasize (bib-format-field x "title"))
	     ,@(if (bib-empty? x "crossref")
		   `(,(bib-format-bvolume x)
		     ,(bib-format-number-series x)
		     ,(bib-format-field x "publisher")
		     ,(bib-format-field x "address")
		     ,(bib-format-edition x)
		     ,(bib-format-date x))
		   `((concat ,(bib-translate "in ")
			     (cite ,(bib-field x "crossref")))
		     ,(bib-format-field x "edition")
		     ,(bib-format-date x))))))
	,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-booklet n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field-Locase x "title")))))
	 ,(bib-new-case-preserved-block
	   (bib-new-case-preserved-sentence
             `(,(bib-format-field-preserve-case x "howpublished")
	       ,(bib-format-field x "address")
	       ,(bib-format-date x))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-inbook n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(if (bib-empty? x "author") (bib-format-editor x) (bib-format-author x))
	      ,(bib-emphasize (bib-format-field x "title"))
	      ,@(if (bib-empty? x "crossref")
		    `(,(bib-format-bvolume x)
		      ,(bib-format-number-series x)
		      ,(bib-format-field x "publisher")
		      ,(bib-format-field x "address")
		      ,(bib-format-edition x)
		      ,(bib-format-date x)
		      ,(bib-format-chapter-pages x))
		    `((concat ,(bib-translate "in ")
			      (cite ,(bib-field x "crossref")))
		      ,(bib-format-date x))))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-incollection n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field-Locase x "title"))
	      ,@(if (bib-empty? x "crossref")
		    `(,(bib-format-in-ed-booktitle x)
		      ,(bib-format-bvolume x)
		      ,(bib-format-number-series x)
		      ,(bib-format-field x "publisher")
		      ,(bib-format-field x "address")
		      ,(bib-format-edition x)
		      ,(bib-format-date x)
		      ,(bib-format-chapter-pages x))
		    `((concat ,(bib-translate "in ")
			      (cite ,(bib-field x "crossref")))
		      ,(bib-format-chapter-pages x))))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-inproceedings n x)
  (:mode bib-siam?)
  `(concat
    ,(bib-format-bibitem n x)
    ,(bib-label (list-ref x 2))
    ,(bib-new-list-spc
      `(,(bib-new-block
	  (bib-new-sentence
	   `(,(bib-format-author x)
	     ,(bib-emphasize (bib-format-field-Locase x "title"))
	     ,@(if (bib-empty? x "crossref")
		   `(,(bib-format-in-ed-booktitle x)
		     ,(bib-format-bvolume x)
		     ,(bib-format-number-series x)
		     ,(bib-format-field x "address")
		     ,(bib-format-date x)
		     ,(bib-format-field x "organization")
		     ,(bib-format-field x "publisher")
		     ,(bib-format-pages x))
		   `((concat ,(bib-translate "in ")
			     (cite ,(bib-field x "crossref")))
		     ,(bib-format-pages x))))))
	,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-manual n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,@(if (bib-empty? x "author")
		    (if (bib-empty? x "organization")
			`()
			`(,(bib-format-field x "organization")
			  ,(bib-format-field x "address")))
		    `(,(bib-format-author x)))
	      ,(bib-emphasize (bib-format-field x "title")))))
	 ,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-field x "organization")
	      ,(bib-format-field x "address")
	      ,(bib-format-edition x)
	      ,(bib-format-date x))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-mastersthesis n x)
  (:mode bib-siam?)
  `(concat
    ,(bib-format-bibitem n x)
    ,(bib-label (list-ref x 2))
    ,(bib-new-list-spc
      `(,(bib-new-block
	  (bib-new-sentence
	   `(,(bib-format-author x)
	     ,(bib-emphasize (bib-format-field-Locase x "title"))
	     ,(if (bib-empty? x "type")
		  (bib-translate "Master's thesis")
		  (bib-format-field x "type"))
	     ,(bib-format-field x "school")
	     ,(bib-format-field x "address")
	     ,(bib-format-date x))))
	,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-misc n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field-Locase x "title")))))
	 ,(bib-new-case-preserved-block
	   (bib-new-case-preserved-sentence
	    `(,(bib-format-field-preserve-case x "howpublished")
	      ,(bib-format-date x))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-phdthesis n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field x "title"))
	      ,(if (bib-empty? x "type")
		   (bib-translate "Master's thesis")
		   (bib-format-field x "type"))
	      ,(bib-format-field x "school")
	      ,(bib-format-field x "address")
	      ,(bib-format-date x))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-proceedings n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(if (bib-empty? x "editor")
		   (bib-format-field x "organization")
		   (bib-format-editor x))
	      ,(bib-emphasize (bib-format-field x "title"))
	      ,(bib-format-bvolume x)
	      ,(bib-format-number-series x)
	      ,(bib-format-field x "address")
	      ,(bib-format-date x)
	      ,(bib-format-field x "organization")
	      ,(bib-format-field x "publisher"))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-techreport n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field-Locase x "title"))
	      ,(bib-format-tr-number x)
	      ,(bib-format-field x "institution")
	      ,(bib-format-field x "address")
	      ,(bib-format-date x))))
	 ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-unpublished n x)
  (:mode bib-siam?)
  `(concat
     ,(bib-format-bibitem n x)
     ,(bib-label (list-ref x 2))
     ,(bib-new-list-spc
       `(,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-author x)
	      ,(bib-emphasize (bib-format-field-Locase x "title")))))
	 ,(bib-new-block
	   (bib-new-sentence
	    `(,(bib-format-field x "note")
	      ,(bib-format-date x))))))))

