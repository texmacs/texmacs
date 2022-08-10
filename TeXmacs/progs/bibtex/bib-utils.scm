
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-utils.scm
;; DESCRIPTION : helper functions for BibTeX styles
;; COPYRIGHT   : (C) 2010, 2015  David MICHEL, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex bib-utils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private administrative functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define bib-current-prefix "bib")
(tm-define bib-style "plain")
(tm-define bib-default-style "plain")

(tm-define (bib-label what)
  `(label ,(string-append bib-current-prefix "-" what)))

(tm-define (bib-preprocessing t)
  (noop))

(tm-define (bib-sort-key x)
  "")

(tm-define (bib-mode? s)
  (or (equal? bib-style s) (equal? bib-default-style s)))

(define (format-entries n x)
  (if (and (list? x) (nnull? x))
      (cons (bib-format-entry n (car x)) (format-entries (+ n 1) (cdr x)))
      `()))

(define (bib-with-sort-key t)
  (if (null? t) `()
      (cons `(,(bib-sort-key (car t)) ,(car t)) (bib-with-sort-key (cdr t)))))

(define (bib-without-sort-key t)
  (if (null? t) `()
      (cons (cadar t) (bib-without-sort-key (cdr t)))))

(define (bib-compare x y)
  (tmstring-before? (car x) (car y)))

(tm-define (bib-sorted-entries l)
  ;; redefine when (e.g.) sorting should be disabled
  (with is-entry? (lambda (x) (func? x 'bib-entry))
    (with l1 (list-filter l is-entry?)
      (with l2 (sort! (bib-with-sort-key l1) bib-compare)
        (bib-without-sort-key l2)))))

(tm-define (bib-process prefix style t)
  (set! bib-current-prefix prefix)
  (set! bib-style style)
  (bib-preprocessing (cdr t))
  (if (and (list? t) (func? t 'document))
      (with ts (bib-sorted-entries (cdr t))
	(bib-simplify
	 `(bib-list
	   ,(number->string (length ts))
	   (document ,@(format-entries 1 ts)))))))

(tm-define bib-functions-table (make-hash-table 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib-standard-styles)
  (list "tm-plain" "tm-abbrv" "tm-abstract" "tm-acm" "tm-alpha" "tm-elsart-num"
        "tm-ieeetr" "tm-siam" "tm-unsrt"))

(tm-define-macro (bib-define-style s d)
  (if (equal? s d)
      `(begin
	 (set! bib-default-style ,s)
	 (texmacs-modes (,(string->symbol (string-append "bib-" s "%"))
			 (bib-mode? ,s))))
      `(begin
	 (set! bib-default-style ,d)
	 (texmacs-modes (,(string->symbol (string-append "bib-" s "%"))
			 (bib-mode? ,s)
			 ,(string->symbol (string-append "bib-" d "%")))))))

(tm-define (bib-with-style s f . args)
  (with tmp-s bib-style
    (set! bib-style s)
    (with res (apply f args)
      (set! bib-style tmp-s)
      res)))

(tm-define (bib-car x)
  (if (pair? x) (car x) ""))

(tm-define (bib-cdr x)
  (if (pair? x) (cdr x) ""))

(tm-define (bib-null? x)
  (cond
    ((tm-func? x 'with) (bib-null? (tm-ref x :last)))
    ((list? x) (equal? x `()))
    ((string? x) (equal? x ""))
    ((symbol? x) (equal? x '#{}#))
    (else #f)))

(tm-define (bib-simplify x)
  (tree->stree (tree-simplify (stree->tree x))))

(tm-define (bib-new-block x)
  (if (bib-null? x) ""
      `(concat ,(bib-add-period (bib-upcase-first x)) (newblock))))

(tm-define (bib-new-case-preserved-block x)
  (if (bib-null? x) ""
      `(concat ,(bib-add-period x) (newblock))))

(define (elim-empty x)
  (if (bib-null? x) `()
      (if (bib-null? (car x)) (elim-empty (cdr x))
	  `(,(car x) ,@(elim-empty (cdr x))))))

(tm-define (new-list-rec s x)
  ;; redefined in ieeetr.scm
  (cond ((bib-null? x) "")
        ((bib-null? (car x)) (new-list-rec s (cdr x)))
        ((null? (cdr x)) `(concat ,(car x)))
        ((and (tm-func? (car x) 'concat) (== (cAr (car x)) `(newblock)))
         `(concat ,(cDr (car x)) ,s (newblock) ,(new-list-rec s (cdr x))))
        (else `(concat ,(car x) ,s ,(new-list-rec s (cdr x))))))

(tm-define (bib-new-list-spc x)
  (new-list-rec " " (elim-empty x)))

(tm-define (bib-new-list c x)
  (new-list-rec c (elim-empty x)))

(tm-define (bib-new-case-preserved-sentence x)
  (bib-add-period (bib-new-list ", " x)))

(tm-define (bib-new-sentence x)
  (bib-add-period (bib-upcase-first (bib-new-list ", " x))))

(tm-define (bib-default-field x s)
  (with e (bib-field x s)
    (if (bib-null? e) e (bib-default-upcase-first e))))

(tm-define (bib-format-field x s)
  (with e (bib-field x s)
    (if (bib-null? e) "" (bib-default-upcase-first e))))

(tm-define (bib-format-field-preserve-case x s)
  (with e (bib-field x s)
    (if (bib-null? e) "" (bib-default-preserve-case e))))

(tm-define (bib-format-field-locase-first x s)
  (with e (bib-field x s)
    (if (bib-null? e) "" (bib-locase-first e))))

(tm-define (bib-format-field-Locase x s)
  (with e (bib-field x s)
    (if (bib-null? e) "" (bib-upcase-first (bib-locase e)))))

(tm-define (bib-emphasize x)
  `(with "font-shape" "italic" ,x))

(tm-define (bib-translate s) `(localize ,s))

(tm-define bib-range-symbol "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-first-last t)
  (:secure #t)
  (if (tree-compound? t) t
      (let* ((s (tree->string t))
	     (i (string-search-forwards " " 0 s))
	     (m (number->string i))
	     (e (number->string (string-length s))))
	(if (< i 0) t
	    `(concat (range ,t "0" ,m) (name (range ,t ,m ,e)))))))
