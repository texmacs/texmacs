
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-edit.scm
;; DESCRIPTION : editing source code
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source source-edit)
  (:use (generic document-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing command tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-latex)
  (make 'latex)
  (set-message "Type a latex command followed by return" "latex"))

(tm-define (activate-compound)
  (with-innermost t 'compound
    (when (not (tree-is? t :up 'inactive))
      (tree-set! t `(inactive ,t)))
    (activate)))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'hybrid))
  (activate-hybrid #f))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'compound))
  (activate-compound))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'latex))
  (activate-latex))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'symbol))
  (activate-symbol))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'inactive))
  (activate))

(tm-define (kbd-variant t forwards?)
  (:require (tree-is? t 'hybrid))
  (activate-hybrid #t))

(tm-define (kbd-variant t forwards?)
  (:require (tree-in? t '(inactive tuple attr)))
  (insert-argument forwards?))

(tm-define (kbd-variant t forwards?)
  (:require (in-source?))
  (insert-argument forwards?))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (tree-is? t 'hybrid))
  (activate-hybrid #t))

(tm-define (inactive-toggle t)
  (if (or (tree-is? t 'inactive) (tree-is? t :up 'inactive))
      (activate)
      (with t-copy t
        (tree-set t `(inactive ,t))
        (notify-disactivated t-copy))))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'hybrid) (tree-is? t :up 'inactive)))
  (activate-hybrid #f))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'compound) (tree-is? t :up 'inactive)))
  (activate-compound))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'latex) (tree-is? t :up 'inactive)))
  (activate-latex))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'symbol) (tree-is? t :up 'inactive)))
  (activate-symbol))

(tm-define (remove-unary-document)
  (with-innermost doc 'document
    (when (and (== (tree-arity doc) 1)
               (> (length (tree->path doc)) 1))
      (tree-remove-node! doc 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting a style file or package from the current file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extract-source-title style?)
  (let* ((tag (if style? 'src-style-file 'src-package))
	 (what (if style? "style file" "style package"))
	 (purpose (string-append "Automatically generated " what "."))
	 (name (url-basename (current-buffer))))
    `(active*
       (document
	 (src-title
	   (document
	     (,tag ,(string-append name "-macros") "1.0")
	     (src-purpose ,purpose)))))))

(define (extract-use-package style?)
  (with l (get-style-list)
    (when (and (not style?) (nnull? l))
      (set! l (cdr l)))
    (if (null? l) (list) (list `(use-package ,@l)))))

(define (init<=? a b)
  (and (tm-func? a 'associate 2) (tm-func? b 'associate 2)
       (string? (tm-ref a 0)) (string? (tm-ref b 0))
       (string<=? (tm-ref a 0) (tm-ref b 0))))

(define (inits->assignments l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (inits->assignments (cdr l))))
	(cond ((not (and (tm-func? head 'associate 2)
			 (string? (tm-ref head 0))))
	       tail)
	      ((in? (tm-ref head 0) (list "zoom-factor" "sfactor"
					  "preamble"
					  "page-screen-width"
					  "page-screen-height"))
	       tail)
	      (else (cons `(assign ,(tm-ref head 0) ,(tm-ref head 1))
			  tail))))))

(define (source-comment s)
  `(active* (document (src-comment (document ,s)))))

(define (extract-style-parameters)
  (let* ((inits (cdr (tm->stree (get-all-inits))))
	 (sorted (sort inits init<=?))
	 (pars (inits->assignments sorted)))
    (if (null? pars) (list)
	(cons (source-comment "Style parameters.") pars))))

(define (extract-macro-definitions)
  (with body (buffer-get-body (current-buffer))
    (if (and (tree-is? body 'document)
	     (tree-in? body 0 '(show-preamble hide-preamble))
	     (tree-is? body 0 0 'document))
	(cons (source-comment "Macro definitions.")
	      (tree-children (tree-ref body 0 0)))
	(list))))

(tm-define (extract-style-file style?)
  (let* ((tit (extract-source-title style?))
	 (packs (extract-use-package style?))
	 (inits (extract-style-parameters))
	 (defs (extract-macro-definitions))
	 (body `(document ,tit ,@packs ,@inits ,@defs))
	 (doc `(document
		 (TeXmacs ,(texmacs-version))
		 (style (tuple "source"))
		 (body ,body))))
    (new-buffer)
    (delayed
      (:idle 1)
      (buffer-set (current-buffer) doc))))
