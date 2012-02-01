
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc.scm
;; DESCRIPTION : generation of larger pieces of documentation
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmdoc-down level)
  (cond ((== level 'title) 'chapter)
	((== level 'part) 'chapter)
	((== level 'tmdoc-title) 'section)
	((== level 'tmdoc-title*) 'section)
	((== level 'chapter) 'section)
	((== level 'appendix) 'section)
	((== level 'section) 'subsection)
	((== level 'subsection) 'subsubsection)
	((== level 'subsubsection) 'paragraph)
	(else 'subparagraph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main expansions routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmdoc-branch x base-name level done)
  (let* ((name (caddr x))
	 (rel-name (url-relative base-name name)))
    (tmdoc-expand rel-name level done)))

(define (tmdoc-substitute-sub l base-name)
  (if (null? l) l
      (cons (tmdoc-substitute (car l) base-name)
	    (tmdoc-substitute-sub (cdr l) base-name))))

(define (tmdoc-substitute x base-name)
  (cond ((match? x '(hyper-link :%1))
	 (list 'hyper-link (cadr x)
	       (url->string (url-relative base-name (caddr x)))))
	((list? x) (cons (car x) (tmdoc-substitute-sub (cdr x) base-name)))
	(else x)))

(define (tmdoc-rewrite-one x base-name the-level done)
  (let* ((omit? (list? the-level))
	 (level (if omit? (car the-level) the-level)))
    (cond ((or (func? x 'tmdoc-title) (func? x 'tmdoc-title*))
	   (if omit? '(document) (cons level (cdr x))))
	  ((func? x 'tmdoc-license)
	   '(document))
	  ((func? x 'traverse)
	   (cons 'document (tmdoc-rewrite (cdadr x) base-name level done)))
	  ((match? x '(branch :%2))
	   (tmdoc-branch x base-name (tmdoc-down level) done))
	  ((match? x '(continue :%2))
	   (tmdoc-branch x base-name (list level) done))
	  ((match? x '(extra-branch :%2))
	   (tmdoc-branch x base-name 'appendix done))
	  ((match? x '(tmdoc-copyright :*))
	   '(document))
	  (else (tmdoc-substitute x base-name)))))

(define (tmdoc-rewrite l base-name level done)
  (if (null? l) l
      (let ((d1 (tmdoc-rewrite-one (car l) base-name level done))
	    (d2 (tmdoc-rewrite (cdr l) base-name level done)))
	(if (func? d1 'document) (append (cdr d1) d2) (cons d1 d2)))))

(define (tmdoc-expand file-name level . opts)
  ;;(display* "tmdoc-expand " file-name "\n")
  (let* ((done (if (null? opts) (make-ahash-table) (car opts)))
	 (done? (ahash-ref done file-name)))
    (ahash-set! done file-name #t)
    (if done?
	'(document "")
	(with t (tree->stree (texmacs-load-tree file-name "texmacs"))
	  (if (string? t)
	      (begin
		(display* "TeXmacs] bad link or file " file-name "\n")
		'(document ""))
	      (with u (cadr (assoc 'body (cdr t)))
		(cons 'document
		      (tmdoc-rewrite (cdr u) file-name level done))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmdoc-search-env-var t which)
  (cond ((nlist? t) #f)
	((null? t) #f)
	((match? t '(associate "language" :%1)) (caddr t))
	(else (let ((val (tmdoc-search-env-var (car t) which)))
		(if val val (tmdoc-search-env-var (cdr t) which))))))

(define (tmdoc-language file-name)
  (let* ((t (texmacs-load-tree file-name "texmacs"))
	 (init (assoc 'initial (cdr (tree->stree t))))
	 (lan (and init (tmdoc-search-env-var (cadr init) "language"))))
    (if lan lan "english")))

(define (tmdoc-get-aux-title doc)
  (cond ((or (nlist? doc) (null? doc)) doc)
	((func? (car doc) 'title) (car doc))
	(else (tmdoc-get-aux-title (cdr doc)))))

(define (tmdoc-get-aux-body doc)
  (cond ((or (nlist? doc) (null? doc)) doc)
	((func? (car doc) 'title) (cdr doc))
	(else (tmdoc-get-aux-body (cdr doc)))))

(define (tmdoc-add-aux doc)
  (cons* 'document
	 (tmdoc-get-aux-title doc)
	 '(table-of-contents toc (document ""))
	 (rcons (tmdoc-get-aux-body doc)
		'(the-index idx (document "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-expand-help file-name level)
  (if (== level 'title)
      (let* ((body (tmdoc-expand file-name level))
	     (lan (tmdoc-language file-name))
	     (doc `(document
		    (style "tmmanual")
		    (body ,(tmdoc-add-aux body))
		    (initial (collection (associate "language" ,lan)
					 (associate "page-medium" "paper"))))))
	(set-help-buffer file-name (stree->tree doc)))
      (let* ((body (tmdoc-expand file-name level))
	     (lan (tmdoc-language file-name))
	     (doc `(document
		    (style "tmdoc")
		    (body ,body)
		    (initial (collection (associate "language" ,lan))))))
	(set-help-buffer file-name (stree->tree doc)))))

(tm-define (delayed-update nr cont)
  (system-wait "Generating automatic content" nr)
  (generate-all-aux)
  (update-buffer)
  (user-delayed cont))

(tm-define (tmdoc-expand-help-manual file-name)
  (system-wait "Generating manual" "(can be long)")
  (tmdoc-expand-help file-name 'title)
  (user-delayed (lambda ()
  (delayed-update "(pass 1/3)" (lambda ()
  (delayed-update "(pass 2/3)" (lambda ()
  (delayed-update "(pass 3/3)" (lambda ()
  (pretend-save-buffer)
  (system-wait "Finishing manual" "(soon ready)"))))))))))

(tm-define (tmdoc-expand-this level)
  (tmdoc-expand-help (get-name-buffer) level))

(define (tmdoc-remove-hyper-links l)
  (cond ((npair? l) l)
	((match? l '(hyper-link :%1)) (cadr l))
	(else (cons (tmdoc-remove-hyper-links (car l))
		    (tmdoc-remove-hyper-links (cdr l))))))

(tm-define (tmdoc-include file-name)
  (let* ((body (tmdoc-expand (tree->string file-name) 'chapter))
	 (filt (list-filter body (lambda (x) (not (func? x 'chapter))))))
    (stree->tree (tmdoc-remove-hyper-links filt))))
