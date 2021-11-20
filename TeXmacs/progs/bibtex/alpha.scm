
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

(define (bib-format-label-year x)
  (if (bib-empty? x "year") ""
      (let* ((y (bib-field x "year"))
             (l (string-length y)))
            (if (<= l 2) y (substring y (- l 2) l)))))

(define (bib-format-label-names a)
  (if (or (bib-null? a) (nlist? a)) ""
    (let* ((n (length a))
         (pre (cond
                ((equal? n 2)
                 (with von (bib-purify (bib-abbreviate
                                        (list-ref (list-ref a 1) 2) "" ""))
                   (if (bib-null? von)
                       (bib-prefix (list-ref (list-ref a 1) 3) 3)
                       (string-append von (bib-prefix
                                           (list-ref (list-ref a 1) 3) 1)))))
                (else
                  (with lab ""
                    (do
                        ((i 1 (+ 1 i)))
                        ((>= i (min n (if (= 5 n) 5 4))))
                      (with von (bib-purify (bib-abbreviate
                                             (list-ref (list-ref a i) 2)
                                             "" ""))
                        (set! lab (string-append
                                   lab von (bib-prefix
                                            (list-ref (list-ref a i) 3) 1)))))
                     lab)))))
      (if (> n 5) (string-append pre "+") pre))))

(define (bib-format-book-inbook-label n x)
  (with key (list-ref x 2)
    (if (bib-empty? x "author")
      (if (bib-empty? x "editor")
        (if (bib-null? key)
          (number->string n)
        (bib-prefix key 3))
            (bib-format-label-names (bib-field x "editor")))
    (bib-format-label-names (bib-field x "author")))))
    
(define (bib-format-proceedings-misc-label ae n x)
  (with key (list-ref x 2)
    (if (bib-empty? x ae)
        (if (bib-null? key)
            (number->string n)
            (bib-prefix key 3))
        (bib-format-label-names (bib-field x ae)))))

(define (bib-format-label-prefix n x)
  (let* ((doctype (list-ref x 1))
         (pre (cond
                ((or (equal? doctype "book") (equal? doctype "inbook"))
                 (bib-format-book-inbook-label n x))
                ((equal? doctype "proceedings")
                 (bib-format-proceedings-misc-label "editor" n x))
                (else (bib-format-proceedings-misc-label "author" n x)))))
    (string-append pre (bib-format-label-year x))))

(define bib-label-table `())
(define bib-key-table `())

(tm-define (bib-preprocessing t)
  (:mode bib-alpha?)
  (set! bib-label-table (make-hash-table 100))
  (set! bib-key-table (make-hash-table 100))
  (do ((entry t (cdr entry)) (n 1 (+ n 1)))
      ((null? entry))
      (if (func? (car entry) 'bib-entry)
          (let* ((label (bib-format-label-prefix 0 (car entry)))
                 (num (ahash-ref bib-label-table label)))
            (ahash-set! bib-key-table (list-ref (car entry) 2) label)
            (if num
                (ahash-set! bib-label-table label
                           (if (equal? num `()) `(1 2) 
                               `(,@num ,(+ 1 (length num)))))
                (ahash-set! bib-label-table label `()))))))

(define (bib-format-label n x)
  (let* ((pre (ahash-ref bib-key-table (list-ref x 2)))
         (num (ahash-ref bib-label-table pre)))
    (if (null? num) pre
        (with n (car num)
          (ahash-set! bib-label-table pre (cdr num))
          (string-append pre (string (integer->char (+ 96 n))))))))

(tm-define (bib-format-bibitem n x)
  (:mode bib-alpha?)
  `(bibitem* ,(bib-format-label n x)))

(define (invert-label l)
  (with invert (lambda (c)
                 (cond
                   ((char-upper-case? c) (char-downcase c))
                   ((char-lower-case? c) (char-upcase c))
                   (else c)))
    (string-map invert l)))

(tm-define (bib-sort-key x)
  (:mode bib-alpha?)
  (let* ((auths (bib-format-label-names (bib-field x "author")))
         (label (ahash-ref bib-key-table (list-ref x 2)))
         (year (bib-field x "year"))
               (lplain (bib-with-style "plain" bib-sort-key x)))
    (string-append (string-upcase (if (bib-null? auths) label auths))
                   (if (bib-null? year) "" year) "    " lplain)))
