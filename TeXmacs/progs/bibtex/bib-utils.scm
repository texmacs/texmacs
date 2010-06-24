
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-utils.scm
;; DESCRIPTION : helper functions for BibTeX styles
;; COPYRIGHT   : (C) 2010  David MICHEL
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

(tm-define bib-style "plain")
(tm-define bib-default-style "plain")

(tm-define (bib-mode? s)
  (or (equal? bib-style s) (equal? bib-default-style s)))

(define (format-entries n x)
  (if (and (list? x) (nnull? x))
      (cons (format-entry n (car x)) (format-entries (+ n 1) (cdr x)))
      `()))

(define (bib-with-sort-key t)
  (if (null? t) `()
      (cons `(,(bib-sort-key (car t)) ,(car t)) (bib-with-sort-key (cdr t)))))

(define (bib-without-sort-key t)
  (if (null? t) `()
      (cons (cadar t) (bib-without-sort-key (cdr t)))))

(define (bib-compare x y)
  (string<? (car x) (car y)))

(define (bib-sorted-entries t)
  (bib-without-sort-key (stable-sort (bib-with-sort-key t) bib-compare)))

(tm-define (bibstyle style t)
  (set! bib-style style)
  (bib-preprocessing (cdr t))
  (if (and (list? t) (func? t 'document))
      (with ts (bib-sorted-entries (cdr t))
	(simplify
	 `(bib-list
	   ,(number->string (length ts))
	   (document ,@(format-entries 1 ts)))))))

(tm-define bib-functions-table (make-hash-table 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(tm-define (empty? x)
  (cond
    ((list? x) (equal? x `()))
    ((string? x) (equal? x ""))
    ((symbol? x) (equal? x '#{}#))
    (else #f)))

(tm-define (simplify x)
  (tree->stree (tree-simplify (stree->tree x))))

(tm-define (new-block x)
  (if (empty? x) ""
      `(concat ,(bib-add-period (bib-upcase-first x)) (newblock))))

(define (elim-empty x)
  (if (empty? x) `()
      (if (empty? (car x)) (elim-empty (cdr x))
	  `(,(car x) ,@(elim-empty (cdr x))))))

(define (new-list-rec s x)
  (if (empty? x) ""
      (if (empty? (car x))
	  (new-list-rec s (cdr x))
	  `(concat ,(car x) ,@(if (nnull? (cdr x))
				  `(,s ,(new-list-rec s (cdr x))) `())))))

(tm-define (new-list-spc x)
  (new-list-rec " " (elim-empty x)))

(tm-define (new-list c x)
  (new-list-rec c (elim-empty x)))

(tm-define (new-sentence x)
  (bib-add-period (bib-upcase-first (new-list ", " x))))

(tm-define (format-field x s)
  (with e (bib-field x s)
    (if (empty? e) "" (bib-default e))))

(tm-define (format-field-Locase x s)
  (with e (bib-field x s)
    (if (empty? e) "" (bib-upcase-first (bib-locase e)))))

(tm-define (emphasize x)
  `(with "font-shape" "italic" ,x))

(tm-define (bib-translate s)
  (translate s "english" (get-env "language")))

