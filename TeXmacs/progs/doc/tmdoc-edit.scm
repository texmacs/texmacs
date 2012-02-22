
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-text-edit.scm
;; DESCRIPTION : editing routines for text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-edit)
  (:use (utils library tree)
	(utils edit variants)
	(text std-text-edit)
	(doc tmdoc-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting meta data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-propose-title?)
  (with buf (buffer-tree)
    (and (in-manual?)
	 (tree-is? buf 'document)
	 (not (previous-section))
	 (not (tree-is? buf 0 'tmdoc-title)))))

(tm-define (tmdoc-propose-copyright-and-license?)
  (with buf (buffer-tree)
    (and (in-manual?)
	 (tree-is? buf 'document)
	 (not (previous-section))
	 (tree-is? buf 0 'tmdoc-title)
	 (not (tree-is? buf :last 'tmdoc-license)))))

(tm-define (tmdoc-insert-title)
  (with buf (buffer-tree)
    (when (tree-is? buf 'document)
      (tree-insert! buf 0 '((tmdoc-title "")))
      (tree-go-to buf 0 0 0))))

(tm-define (tmdoc-insert-copyright)
  (with buf (buffer-tree)
    (when (tree-is? buf 'document)
      (with n (- (tree-arity buf)
                 (if (tree-in? buf :last '(tmdoc-license
                                           tmweb-license)) 1 0))
        (tree-insert! buf n '((tmdoc-copyright "" "")))
        (tree-go-to buf n 0 0)))))

(tm-define (tmdoc-insert-license)
  (with buf (buffer-tree)
    (when (tree-is? buf 'document)
      (with n (tree-arity buf)
        (tree-insert! buf n '((tmdoc-license "")))
        (tree-go-to buf n 0 0)))))

(tm-define (tmdoc-insert-gnu-fdl)
  (with buf (buffer-tree)
    (when (tree-is? buf 'document)
      (with n (tree-arity buf)
        (tree-insert! buf n '((tmdoc-license "")))
        (tree-go-to buf n 0 0)
        (insert "Permission is granted to copy, distribute and/or modify this
document under the terms of the GNU Free Documentation License, Version 1.1 or
any later version published by the Free Software Foundation; with no Invariant
Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of
the license is included in the section entitled \"GNU Free Documentation License\".")))))

(tm-define (tmdoc-insert-copyright-and-license)
  (tmdoc-insert-gnu-fdl)
  (tmdoc-insert-copyright))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting meta data for TeXmacs web pages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmweb-propose-title?)
  (with buf (buffer-tree)
    (and (in-tmweb?)
	 (tree-is? buf 'document)
	 (not (previous-section))
	 (not (tree-is? buf 0 'tmweb-title)))))

(tm-define (tmweb-propose-copyright-and-license?)
  (with buf (buffer-tree)
    (and (in-manual?)
	 (tree-is? buf 'document)
	 (not (previous-section))
	 (tree-is? buf 0 'tmweb-title)
	 (not (tree-is? buf :last 'tmweb-license)))))

(tm-define (tmweb-insert-title)
  (with buf (buffer-tree)
    (when (tree-is? buf 'document)
      (tree-insert! buf 0 '((tmweb-title "" "")))
      (tree-go-to buf 0 0 0))))

(tm-define (tmweb-insert-license)
  (with buf (buffer-tree)
    (when (tree-is? buf 'document)
      (with n (tree-arity buf)
        (tree-insert! buf n '((tmweb-license)))))))

(tm-define (tmweb-insert-copyright-and-license)
  (tmweb-insert-license)
  (tmdoc-insert-copyright))

(tm-define (tmweb-insert-classifiers main sub)
  (:interactive #t)
  (:argument main "Main classification")
  (:argument sub "Subclassification")
  (init-env "tmweb-main" main)
  (init-env "tmweb-sub" sub)
  (and-with tit (tree-ref (buffer-tree) 0)
    (when (tree-is? tit 'tmweb-title)
      (and-with links (tree-ref tit 1)
        (with s (string-append "tmweb-" (locase-all main) "-links")
          (if (== s "tmweb-plug-ins-links") (set! s "tmweb-plugin-links"))
          (tree-set links `(,(string->symbol s))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting branches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-make-branch l)
  (with-innermost t 'traverse
    (with doc (tree-ref t 0)
      (when (and doc (tree-is? doc 'document))
        (with i (tree-down-index doc)
          (when (!= (tree-ref doc i) (string->tree ""))
            (tree-insert! doc (+ i 1) '(""))
            (tree-go-to doc (+ i 1) 0))
          (make l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting the synopsis of an explanation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-insert-explain-synopsis)
  (with-innermost t 'explain
    (when (and t (== (tree-arity t) 2))
      (tree-go-to t 0 :end)
      (make 'explain-synopsis))))
