
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : alpha.scm
;; DESCRIPTION : alpha style for BibTeX files
;; COPYRIGHT   : (C) 2010  David MICHEL
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex alpha)
  (:use (bibtex bib-utils) (bibtex plain)))

(bib-define-style "alpha" "plain")

(define (format-label-year x)
  (let* ((y (bib-field x "year"))
	 (l (string-length y)))
    (if (<= l 2) y (substring y (- l 2) l))))

(define (format-label-names a)
  (let* ((n (length a))
	 (pre (cond
		((equal? n 2)
		 (with von (bib-purify (bib-abbreviate
					(list-ref (list-ref a 1) 2) "" ""))
		   (if (empty? von)
		       (bib-prefix (list-ref (list-ref a 1) 3) 3)
		       (string-append von (bib-prefix
					   (list-ref (list-ref a 1) 3) 1)))))
		(else
		  (with lab ""
		    (do
			((i 1 (+ 1 i)))
			((>= i (min n 5)))
		      (with von (bib-purify (bib-abbreviate
					     (list-ref (list-ref a i) 2)
					     "" ""))
			(set! lab (string-append
				   lab von (bib-prefix
					    (list-ref (list-ref a i) 3) 1)))))
		    lab)))))
    (if (> n 5) (string-append pre "+") pre)))

(define (format-book-inbook-label n x)
  (with key (list-ref x 2)
    (if (bib-empty? x "author")
	(if (bib-empty? x "editor")
	    (if (empty? key)
		(number->string n)
		(bib-prefix key 3))
	    (format-label-names (bib-field x "editor")))
	(format-label-names (bib-field x "author")))))

(define (format-proceedings-misc-label ae n x)
  (with key (list-ref x 2)
    (if (bib-empty? x ae)
	(if (empty? key)
	    (number->string n)
	    (bib-prefix key 3))
	(format-label-names (bib-field x ae)))))

(define (format-label-prefix n x)
  (let* ((doctype (list-ref x 1))
	 (pre (cond
		((or (equal? doctype "book") (equal? doctype "inbook"))
		 (format-book-inbook-label n x))
		((equal? doctype "proceedings")
		 (format-proceedings-misc-label "editor" n x))
		(else (format-proceedings-misc-label "author" n x)))))
    (string-append pre (format-label-year x))))

(define bib-label-table `())
(define bib-key-table `())

(tm-define (bib-preprocessing t)
  (:mode bib-alpha?)
  (set! bib-label-table (make-hash-table 100))
  (set! bib-key-table (make-hash-table 100))
  (do ((entry t (cdr entry)) (n 1 (+ n 1)))
      ((null? entry))
    (let* ((label (format-label-prefix 0 (car entry)))
	   (num (hash-ref bib-label-table label)))
      (hash-set! bib-key-table (list-ref (car entry) 2) label)
      (if num
	  (hash-set! bib-label-table label
		     (if (equal? num `()) `(1 2) `(,@num ,(+ 1 (length num)))))
	  (hash-set! bib-label-table label `())))))

(define (format-label n x)
  (let* ((pre (hash-ref bib-key-table (list-ref x 2)))
	 (num (hash-ref bib-label-table pre)))
    (if (null? num) pre
	(with n (car num)
	  (hash-set! bib-label-table pre (cdr num))
	  (string-append pre (string (integer->char (+ 96 n))))))))

(tm-define (format-bibitem n x)
  (:mode bib-alpha?)
  `(bibitem* ,(format-label n x)))

(define (invert-label l)
  (with invert (lambda (c)
		 (cond
		   ((char-upper-case? c) (char-downcase c))
		   ((char-lower-case? c) (char-upcase c))
		   (else c)))
    (string-map invert l)))

(tm-define (bib-sort-key x)
  (:mode bib-alpha?)
  (let ((label (hash-ref bib-key-table (list-ref x 2)))
	(lplain (bib-with-style "plain" bib-sort-key x)))
    (string-append (invert-label label) "    " lplain)))
