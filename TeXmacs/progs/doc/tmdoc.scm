
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

(define (tmdoc-relative cur name)
  ;;(display* "tmdoc-relative " cur ", " name "\n")
  (with rel (url-relative cur name)
    (if (url-regular? rel) rel
        (let* ((nname (string-append name "." (ext-language-suffix) ".tm"))
               (nfile (url-search-upwards (url-head cur) nname
                                          (list "doc" "web" "texmacs"))))
          (if (not (url-none? nfile)) nfile
              (let* ((ename (string-append name ".en.tm"))
                     (efile (url-search-upwards (url-head cur) ename
                                                (list "doc" "web" "texmacs"))))
                (if (not (url-none? efile)) efile rel)))))))

(define (tmdoc-branch x root cur level done)
  (let* ((name (caddr x))
	 (rel-name (tmdoc-relative cur name)))
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
           (list (car x) (cadr x) (url->unix u2))))
        ((tm-func? x 'if-ref*)
         (cons 'if-ref (tmdoc-substitute-sub (cdr x) root cur)))
        ((tm-func? x 'if-nref*)
         (cons 'if-nref (tmdoc-substitute-sub (cdr x) root cur)))
        ((tm-func? x 'tmdoc-link*)
         (cons 'tmdoc-link (tmdoc-substitute-sub (cdr x) root cur)))
        ((and (tm-in? x '(bibliography bibliography*))
              (tm-atomic? (tm-ref x 2)))
         (let* ((name (tm->string (tm-ref x 2)))
                (rel (if (== name "") "" (url-relative cur name))))
           `(,(tm-label x) ,(tm-ref x 0) ,(tm-ref x 1)
             ,(url->string rel) ,@(cdddr (cDr (tm-children x)))
             ,(tmdoc-substitute (cAr x) root cur))))
	((list? x)
         (cons (car x) (tmdoc-substitute-sub (cdr x) root cur)))
	(else x)))

(define (tmdoc-rewrite-one x root cur the-level done)
  (let* ((omit? (list? the-level))
	 (level (if omit? (car the-level) the-level)))
    (cond ((or (func? x 'tmdoc-title) (func? x 'tmdoc-title*))
	   (if omit? '(document) (cons level (cdr x))))
          ((and (func? x 'concat)
                (or (func? (tm-ref x 0) 'tmdoc-title)
                    (func? (tm-ref x 0) 'tmdoc-title*)))
           `(concat ,@(map (cut tmdoc-rewrite-one <> root cur the-level done)
                           (tm-children x))))
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
	  ((match? x '(optional-branch :%2))
	   '(document))
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
	(with t (tree->stree (tree-import cur "texmacs"))
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

(define (tmdoc-language file-name)
  (with t (tree-import file-name "texmacs")
    (tmfile-language t)))

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

(tmfs-permission-handler (help name type)
  (and (== type "read")
       (let* ((file (or (tmfs-cdr name) ""))
              (root (tmfs-string->url file)))
         (if (or (== file "") (not (url-exists? root)))
             (in? (url-suffix root) (list "html" "tm"))
             #t))))

(tmfs-load-handler (help name)
  (let* ((type (or (tmfs-car name) "normal"))
         (file (or (tmfs-cdr name) ""))
         (root (tmfs-string->url file)))
    (cond ((or (== file "") (not (url-exists? root)))
           `(document
              (TeXmacs ,(texmacs-version))
              (style "tmdoc")
              (body (document
                      "Broken link."
                      (concat "File " (tt ,root) " does not exist")))))
          ((== (url-suffix root) "html")
           (with doc (tm->stree (tree-import root "html"))
             `(document
                (TeXmacs ,(texmacs-version))
                ,@(cdr doc))))
          ((!= (url-suffix root) "tm")
           (string-load root))
          ((== type "normal")
           (tm->stree (tree-import root "texmacs")))
          ((== type "book")
           (let* ((body (tmdoc-expand root root 'title))
                  (lan (tmdoc-language root)))
             (tm->stree
              `(document
                 (TeXmacs ,(texmacs-version))
                 (style (tuple "tmmanual" ,lan))
                 (body ,(tmdoc-add-aux body))
                 (initial (collection (associate "page-medium" "paper")))))))
          (else
           (let* ((body (tmdoc-expand root root 'tmdoc-title))
                  (lan (tmdoc-language root)))
             (tm->stree
              `(document
                 (TeXmacs ,(texmacs-version))
                 (style (tuple "tmdoc" ,lan))
                 (body ,body))))))))

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
      (string-append "tmfs://help/" (url->unix name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-expand-help root type)
  (with name (url->tmfs-string root)
    (when (string-starts? name "tmfs/help/")
      (set! name (string-drop name 10))
      (set! name (tmfs-cdr name)))
    (load-document (string-append "tmfs://help/" type "/" name))))

(tm-define (delayed-update nr cont)
  (system-wait "Generating automatic content" nr)
  (generate-all-aux)
  (update-current-buffer)
  (user-delayed cont))

(tm-define (tmdoc-expand-help-manual* root next)
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
                  (next))))))))))

(tm-define (tmdoc-expand-help-manual root)
  (tmdoc-expand-help-manual*
   root (lambda () (system-wait "Finishing manual" "(soon ready)"))))

(tm-define (tmdoc-expand-this* type next)
  (system-wait (string-append "Generating " type) "(can be long)")
  (with mmx? (style-has? "mmxdoc-style")
    (tmdoc-expand-help (current-buffer) type)
    (if mmx? (set-main-style "mmxmanual"))
    (user-delayed
      (lambda ()
        (delayed-update "(pass 1/3)"
          (lambda ()
            (delayed-update "(pass 2/3)"
              (lambda ()
                (delayed-update "(pass 3/3)"
                  (lambda ()
                    (buffer-pretend-saved (current-buffer))
                    (next)))))))))))

(tm-define (tmdoc-expand-this type)
  (tmdoc-expand-this*
   type (lambda () (system-wait "Finishing manual" "(soon ready)"))))

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
