
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

(define (tmdoc-branch x root cur level done)
  (let* ((name (caddr x))
	 (rel-name (url-relative cur name)))
    (tmdoc-expand root rel-name level done)))

(define (tmdoc-substitute-sub l root cur)
  (if (null? l) l
      (cons (tmdoc-substitute (car l) root cur)
	    (tmdoc-substitute-sub (cdr l) root cur))))

(define (tmdoc-substitute x root cur)
  (cond ((or (match? x '(hlink :%2)) (match? x '(hyper-link :%2)))
         (let* ((u1 (url-relative cur (caddr x)))
                (u2 (url-delta root u1)))
           ;;(display* root ", " cur ", " (caddr x) " -> " u2 "\n")
           (list (car x) (cadr x) (url->string u2))))
	((list? x) (cons (car x) (tmdoc-substitute-sub (cdr x) root cur)))
	(else x)))

(define (tmdoc-rewrite-one x root cur the-level done)
  (let* ((omit? (list? the-level))
	 (level (if omit? (car the-level) the-level)))
    (cond ((or (func? x 'tmdoc-title) (func? x 'tmdoc-title*))
	   (if omit? '(document) (cons level (cdr x))))
	  ((func? x 'tmdoc-license)
	   '(document))
	  ((func? x 'traverse)
	   (cons 'document (tmdoc-rewrite (cdadr x) root cur level done)))
	  ((match? x '(branch :%2))
	   (tmdoc-branch x root cur (tmdoc-down level) done))
	  ((match? x '(continue :%2))
	   (tmdoc-branch x root cur (list level) done))
	  ((match? x '(extra-branch :%2))
	   (tmdoc-branch x root cur 'appendix done))
	  ((match? x '(tmdoc-copyright :*))
	   '(document))
	  (else (tmdoc-substitute x root cur)))))

(define (tmdoc-rewrite l root cur level done)
  (if (null? l) l
      (let ((d1 (tmdoc-rewrite-one (car l) root cur level done))
	    (d2 (tmdoc-rewrite (cdr l) root cur level done)))
	(if (func? d1 'document) (append (cdr d1) d2) (cons d1 d2)))))

(define (tmdoc-expand root cur level . opts)
  ;;(display* "tmdoc-expand " cur "\n")
  (let* ((done (if (null? opts) (make-ahash-table) (car opts)))
	 (done? (ahash-ref done cur)))
    (ahash-set! done cur #t)
    (if done?
	'(document "")
	(with t (tree->stree (texmacs-load-tree cur "texmacs"))
	  (if (string? t)
	      (begin
		(display* "TeXmacs] bad link or file " cur "\n")
		'(document ""))
	      (with u (cadr (assoc 'body (cdr t)))
		(cons 'document
		      (tmdoc-rewrite (cdr u) root cur level done))))))))

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

(define (tmdoc-add-aux doc)
  (let* ((l0 (cdr doc))
         (i  (list-find-index l0 (lambda (x) (func? x 'title))))
         (l1 (if i (sublist l0 0 (+ i 1)) '()))
         (l2 (if i (sublist l0 (+ i 1) (length l0)) l0)))
    `(document
       ,@l1
       (table-of-contents "toc" (document ""))
       ,@l2
       (the-index "idx" (document "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (help name)
  (let* ((type (or (tmfs-car name) "normal"))
         (file (or (tmfs-cdr name) ""))
         (root (tmfs-string->url file)))
    (cond ((or (== file "") (not (url-exists? root)))
           `(document
              (TeXmacs ,(texmacs-version))
              (style "tmdoc")
              (body (document "Broken link."))))
          ((!= (url-suffix root) "tm")
           (string-load root))
          ((== type "normal")
           (tm->stree (texmacs-load-tree root "texmacs")))
          ((== type "book")
           (let* ((body (tmdoc-expand root root 'title))
                  (lan (tmdoc-language root)))
             (tm->stree
              `(document
                 (TeXmacs ,(texmacs-version))
                 (style "tmmanual")
                 (body ,(tmdoc-add-aux body))
                 (initial (collection (associate "language" ,lan)
                                      (associate "page-medium" "paper")))))))
          (else
           (let* ((body (tmdoc-expand root root 'tmdoc-title))
                  (lan (tmdoc-language root)))
             (tm->stree
              `(document
                 (TeXmacs ,(texmacs-version))
                 (style "tmdoc")
                 (body ,body)
                 (initial (collection (associate "language" ,lan))))))))))

(define (tmdoc-find-title-list l)
  (and (nnull? l)
       (or (tmdoc-find-title (car l))
           (tmdoc-find-title-list (cdr l)))))

(tm-define (tmdoc-find-title doc)
  (cond ((tm-atomic? doc) #f)
        ((tm-in? doc '(title doc-title tmdoc-title tmdoc-title* tmweb-title))
         (with title (cpp-texmacs->verbatim (tm-ref doc 0) #f "default")
           (string-append "Help - " title)))
        (else (tmdoc-find-title-list (tm-children doc)))))

(tmfs-title-handler (help name doc)
  (or (tmdoc-find-title doc)
      (string-append "tmfs://help/" (url->string name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-expand-help root type)
  (load-buffer (string-append "tmfs://help/" type "/"
                              (url->tmfs-string root))))

(tm-define (delayed-update nr cont)
  (system-wait "Generating automatic content" nr)
  (generate-all-aux)
  (update-buffer)
  (user-delayed cont))

(tm-define (tmdoc-expand-help-manual root)
  (system-wait "Generating manual" "(can be long)")
  (tmdoc-expand-help root "book")
  (user-delayed
    (lambda ()
      (delayed-update "(pass 1/3)"
        (lambda ()
          (delayed-update "(pass 2/3)"
            (lambda ()
              (delayed-update "(pass 3/3)"
                (lambda ()
                  (buffer-pretend-saved (current-buffer))
                  (system-wait "Finishing manual" "(soon ready)"))))))))))

(tm-define (tmdoc-expand-this type)
  (tmdoc-expand-help (current-buffer) type))

(define (tmdoc-remove-hyper-links l)
  (cond ((npair? l) l)
	((match? l '(hyper-link :%1)) (cadr l))
	(else (cons (tmdoc-remove-hyper-links (car l))
		    (tmdoc-remove-hyper-links (cdr l))))))

(tm-define (tmdoc-include incl)
  (let* ((root (tree->string incl))
         (body (tmdoc-expand root root 'chapter))
	 (filt (list-filter body (lambda (x) (not (func? x 'chapter))))))
    (stree->tree (tmdoc-remove-hyper-links filt))))
