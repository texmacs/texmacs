
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : acm.scm
;; DESCRIPTION : acm style for BibTeX files
;; COPYRIGHT   : (C) 2010, 2015  David MICHEL, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex acm)
  (:use (bibtex bib-utils) (bibtex plain)))

(bib-define-style "acm" "plain")

(tm-define (bib-format-name x)
  (:mode bib-acm?)
  (let* ((f (if (bib-null? (list-ref x 1)) ""
		`(concat ", " ,(bib-abbreviate (list-ref x 1) "." `(nbsp)))))
	 (vv (if (bib-null? (list-ref x 2)) ""
                 `(concat ,(list-ref x 2) (nbsp))))
	 (ll (if (bib-null? (list-ref x 3)) ""
                 (bib-purify (list-ref x 3))))
	 (jj (if (bib-null? (list-ref x 4)) ""
                 `(concat ", " ,(list-ref x 4)))))
    `(with "font-shape" "small-caps" (concat ,vv ,ll ,jj ,f))))

(tm-define (bib-format-editor x)
  (:mode bib-acm?)
  (let* ((a (bib-field x "editor")))
    (if (or (bib-null? a) (nlist? a))
	""
	(if (equal? (length a) 2)
	    `(concat ,(bib-format-names a) ,(bib-translate ", Ed."))
	    `(concat ,(bib-format-names a) ,(bib-translate ", Eds."))))))

(tm-define (bib-format-date x)
  (:mode bib-acm?)
  (let* ((y (bib-field x "year"))
	 (m (bib-field x "month")))
    (if (bib-null? y)
	(if (bib-null? m) "" m)
	(if (bib-null? m) y `(concat ,m " " ,y)))))

(tm-define (bib-format-in-ed-booktitle x)
  (:mode bib-acm?)
  (let* ((b (bib-field x "booktitle"))
	 (a (bib-field x "address"))
	 (cl `(concat " (" ,(bib-new-list ", " `(,a ,(bib-format-date x))) ")")))
    (if (bib-null? b)
	""
	`(concat
	  ,(bib-translate "in ")
	  (with "font-shape" "italic" ,b) ,cl))))

(define (bib-format-volume-or-number x)
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
	      `(concat "no." ,sep ,n ,series)))
	(let ((series (if (bib-null? s)
			  ""
			  `(concat ,(bib-translate " of ")
				   (with "font-shape" "italic" ,s))))
	      (sep (if (< (bib-text-length v) 3) `(nbsp) " ")))
	  `(concat "vol." ,sep ,v ,series)))))

(tm-define (bib-format-pages x)
  (:mode bib-acm?)
  (let* ((p (bib-field x "pages")))
    (cond
      ((or (bib-null? p) (nlist? p)) "")
      ((== (length p) 1) "")
      ((== (length p) 2) (list-ref p 1))
      (else `(concat ,(list-ref p 1) ,bib-range-symbol ,(list-ref p 2))))))

(tm-define (bib-format-chapter-pages x)
  (:mode bib-acm?)
  (let* ((c (bib-field x "chapter"))
	 (t (bib-field x "type")))
    (if (bib-null? c)
	(bib-format-pages x)
	(let ((type (if (bib-null? t)
			(bib-translate "chapter")
			(bib-locase t)))
	      (pages `(concat ", " ,(bib-format-pages x))))
	  `(concat ,type " " ,c ,pages)))))

(tm-define (bib-format-tr-number x)
  (:mode bib-acm?)
  (let* ((t (bib-field x "type"))
	 (n (bib-field x "number"))
	 (type (if (bib-null? t) (bib-translate "Technical Report") t))
	 (number (if (bib-null? n) "" n))
	 (sep (if (< (bib-text-length n) 3) `(nbsp) " ")))
    (if (bib-null? n) type
        `(concat ,type ,sep ,number))))

(define (bib-format-edition x)
  (let* ((e (bib-field x "edition")))
    (if (bib-null? e) "" `(concat ,e " ed."))))

(tm-define (bib-format-bibitem n x)
  (:mode bib-acm?)
  `(bibitem* ,(number->string n)))

(define (bib-format-journal-volume-date x)
  (bib-new-sentence
   `((concat ,(bib-emphasize
               `(concat ,(bib-format-field x "journal")
                        ,(if (bib-null? (bib-field x "volume")) "" " ")
                        ,(bib-format-field x "volume")))
             " (" ,(bib-format-date x) ")")
     ,(bib-format-pages x))))

(define (bib-format-journal-volume-number-date x)
  (bib-new-sentence
   `(,(bib-emphasize
       `(concat ,(bib-format-field x "journal")
                ,(if (bib-null? (bib-field x "volume")) "" " ")
                ,(bib-format-field x "volume")))
     (concat ,(bib-format-field x "number")
             " (" ,(bib-format-date x) ")")
     ,(bib-format-pages x))))

(tm-define (bib-format-article n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block (bib-format-author x))
	       ,(bib-new-block (bib-format-field-Locase x "title"))
	       ,(bib-new-block
		 (if (bib-empty? x "crossref")
                     (if (bib-null? (bib-field x "number"))
                         (bib-format-journal-volume-date x)
                         (bib-format-journal-volume-number-date x))
		     (bib-new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-pages x)))))
	       ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-book n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block (if (bib-empty? x "author")
			       (bib-format-editor x)
			       (bib-format-author x)))
	       ,(bib-new-block
		 (bib-new-sentence
		  `(,(bib-emphasize (bib-format-field x "title"))
		    ,(bib-format-edition x))))
	       ,(bib-new-block
		 (if (bib-empty? x "crossref")
		     (bib-new-list-spc
		      `(,(bib-new-sentence `(,(bib-format-number-series x)))
			,(bib-new-sentence
			  `(,(bib-format-field x "publisher")
			    ,(bib-format-field x "address")
			    ,(bib-format-date x)))))
		     (bib-new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-field x "edition")
			,(bib-format-date x)))))
	       ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-inbook n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block (if (bib-empty? x "author")
			       (bib-format-editor x)
			       (bib-format-author x)))
	       ,(bib-new-block
		 (bib-new-sentence
		  `(,(bib-emphasize (bib-format-field x "title")))))
	       ,(bib-new-block
		 (if (bib-empty? x "crossref")
		     (bib-new-list-spc
		      `(,(bib-new-sentence `(,(bib-format-number-series x)))
			,(bib-new-sentence
			  `(,(bib-format-field x "publisher")
			    ,(bib-format-field x "address")
			    ,(bib-format-date x)
			    ,(bib-format-chapter-pages x)))))
		     (bib-new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-field x "edition")
			,(bib-format-date x)))))
	       ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-incollection n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block (bib-format-author x))
	       ,(bib-new-block (bib-format-field-Locase x "title"))
	       ,(bib-new-block
		 (if (bib-empty? x "crossref")
		     (bib-new-list-spc
		      `(,(bib-new-sentence
			  `((concat ,(bib-translate "in ")
				    ,(bib-emphasize (bib-format-field x "booktitle")))
			    ,(bib-format-editor x)
			    ,(bib-format-edition x)
			    ,(bib-format-volume-or-number x)))
			,(bib-new-sentence
			  `(,(bib-format-field x "publisher")
			    ,(bib-format-field x "address")
			    ,(bib-format-date x)
			    ,(bib-format-chapter-pages x)))))
		     (bib-new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-chapter-pages x)))))
	       ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-inproceedings n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block (bib-format-author x))
	       ,(bib-new-block (bib-format-field-Locase x "title"))
	       ,(bib-new-block
		 (if (bib-empty? x "crossref")
		     (bib-new-list-spc
		      `(,(bib-new-sentence
			  `(,(bib-format-in-ed-booktitle x)
			    ,(bib-format-editor x)
			    ,(bib-format-volume-or-number x)
			    ,(bib-format-field x "organization")
			    ,(bib-format-field x "publisher")
			    ,(bib-format-pages x)))))
		     (bib-new-sentence
		      `((concat ,(bib-translate "in ")
				(cite ,(bib-field x "crossref")))
			,(bib-format-pages x)))))
	       ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-manual n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block (bib-format-author x))
	       ,(bib-new-block
		 (bib-new-sentence
		  `(,(bib-emphasize (bib-format-field x "title"))
		    ,(bib-format-edition x)
		    ,(bib-format-field x "organization")
		    ,(bib-format-field x "address")
		    ,(bib-format-date x))))
	       ,(bib-new-block (bib-format-field x "note"))))))

(tm-define (bib-format-proceedings n x)
  (:mode bib-acm?)
  `(concat ,(bib-format-bibitem n x)
	   ,(bib-label (list-ref x 2))
	   ,(bib-new-list-spc
	     `(,(bib-new-block
		 (if (bib-empty? x "editor")
		     (bib-format-field x "organization")
		     (bib-format-editor x)))
	       ,(bib-new-block
		 (bib-new-sentence
		  `((concat ,(bib-emphasize (bib-format-field x "title"))
			    " ("
			    ,(bib-new-list ", "
				       `(,(bib-format-field x "address")
					 ,(bib-format-date x)))
			    ")")
		    ,(bib-format-volume-or-number x)
		    ,(bib-format-field x "organization")
		    ,(bib-format-field x "publisher"))))
	       ,(bib-new-block (bib-format-field x "note"))))))

