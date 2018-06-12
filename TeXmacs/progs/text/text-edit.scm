
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-edit.scm
;; DESCRIPTION : editing routines for text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-edit)
  (:use (utils library tree)
	(utils edit variants)
	(text text-drd)
	(generic format-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style package rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-category p)
  (:require (in? p (list "centered-program" "framed-program")))
  :program-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a title and an abstract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-propose-title?)
  (with bt (buffer-tree)
    (with brothers (map tree-label (tree-children bt))
      (and-with t (tree-ref bt :down)
        (and (tree-is? bt 'document)
	     (match? (cursor-tree) "")
	     (not (in? 'doc-data brothers))
             (not (style-has? "beamer-style")))))))

(tm-define (document-propose-abstract?)
  (with bt (buffer-tree)
    (with brothers (map tree-label (tree-children bt))
      (and-with t (tree-ref bt :down)
        (and (tree-is? bt 'document)
	     (match? (cursor-tree) "")
             (in? 'doc-data brothers)
	     (not (in? 'abstract-data brothers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting document, author and abstract data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-title-context? t)
  (and (tree-search-upwards t 'doc-data)
       (or (tree-in? t (doc-title-tag-list))
           (and (tree-is? t 'date) (tree-is? t :up 'doc-date)))))

(tm-define (doc-author-context? t)
  (and (tree-search-upwards t 'doc-data)
       (tree-in? t (author-data-tag-list))))

(define doc-data-inactive-tags
  (doc-title-inactive-tag-list))

(tm-define (make-doc-data)
  (insert-go-to '(doc-data (doc-title "")) '(0 0 0)))

(tm-define (make-doc-data-element l)
  (with-innermost t 'doc-data
    (with pos (1+ (tree-down-index t))
      (cond ((== l 'doc-author)
	     (tree-insert! t pos `((,l (author-data (author-name "")))))
	     (tree-go-to t pos 0 0 0 0))
	    ((== l 'doc-note)
	     (tree-insert! t pos `((,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    ((== l 'doc-title-options)
	     (tree-insert! t pos `((,l))))
	    ((in? l doc-data-inactive-tags)
	     (tree-insert! t pos `((doc-inactive (,l ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert! t pos `((,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (make-author-data-element l)
  (with-innermost t 'author-data
    (with pos (1+ (tree-down-index t))
      (cond ((in? l '(author-affiliation author-note))
	     (tree-insert! t pos `((,l (document ""))))
	     (tree-go-to t pos 0 0 0))
	    (else
	     (tree-insert! t pos `((,l "")))
	     (tree-go-to t pos 0 0))))))

(tm-define (abstract-data-context? t)
  (tree-in? t (abstract-data-tag-list)))

(tm-define (make-abstract-data)
  (insert-go-to '(abstract-data (abstract "")) '(0 0 0)))

(tm-define (make-abstract-data-element l)
  (with-innermost t 'abstract-data
    (with pos (1+ (tree-down-index t))
      (tree-insert! t pos `((,l "")))
      (tree-go-to t pos 0 0))))

(tm-define (kbd-space-bar t shift?)
  (:require (and (tree-is-buffer? t) (in-text?)
		 (!= (get-env "language") "verbatim")))
  (let* ((b (before-cursor))
	 (p (get-preference "text spacebar")))
    (cond ((== p "allow multiple spaces")
	   (insert " "))
	  ((and (== b " ") (== p "no multiple spaces"))
	   (noop))
	  ((== b " ")
	   (remove-text #f)
	   (make-space "1em"))
	  ((and (tree? b) (tree-func? b 'space 1))
	   (if (and (tree-atomic? (tree-ref b 0))
		    (string-ends? (tree->string (tree-ref b 0)) "em"))
	       (make-space "1em")
	       (geometry-horizontal b #t)))
	  (else (insert " ")))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'title))
  (go-end-line)
  (insert-return))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'doc-title))
  (make-doc-data-element 'doc-author))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'author-name))
  (make-author-data-element 'author-affiliation))

(tm-define (kbd-enter t shift?)
  (:require (or
              (tree-is? t 'abstract-arxiv)
              (tree-is? t 'abstract-pacs)
              (tree-is? t 'abstract-acm)
              (tree-is? t 'abstract-msc)
              (tree-is? t 'abstract-keywords)))
  (with t (tree-search-upwards
            t '(abstract-msc abstract-acm abstract-pacs
                             abstract-arxiv abstract-keywords))
    (with pos (1+ (tree-down-index t))
      (tree-insert! t pos `((concat "")))
      (tree-go-to t pos 0 0))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'doc-inactive))
  (doc-data-activate-here))

(tm-define (set-doc-title-options opts)
  (with-innermost t 'doc-data
    (with opts-trees (select t '(doc-title-options))
      (if (null? opts)
          (when (nnull? opts-trees)
            (with old (car opts-trees)
              (tree-remove (tree-up old) (tree-index old) 1)))
          (begin
            (when (null? opts-trees)
              (make-doc-data-element 'doc-title-options)
              (set! opts-trees (select t '(doc-title-options))))
            (tree-set (car opts-trees) `(doc-title-options ,@opts)))))))

(tm-define (get-doc-title-options)
  (with-innermost t 'doc-data
    (with opts-trees (select t '(doc-title-options :%1))
      (map tree->stree opts-trees))))

(tm-define (test-doc-title-clustering? mode)
  (with cl (list "cluster-all" "cluster-by-affiliation")
    (with old (get-doc-title-options)
      (if mode (in? mode old) (null? (list-intersection cl old))))))

(tm-define (set-doc-title-clustering mode)
  (:check-mark "*" test-doc-title-clustering?)
  (with cl (list "cluster-all" "cluster-by-affiliation")
    (with old (list-difference (get-doc-title-options) cl)
      (set-doc-title-options (if mode (cons mode old) old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activation and disactivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (doc-data-go-to-active t i)
  (cond ((< i 0) (tree-go-to t :end))
	((tree-in? t i (doc-title-inactive-tag-list))
	 (doc-data-go-to-active t (- i 1)))
	((not (cursor-inside? (tree-ref t i)))
	 (tree-go-to t i :end))))

(tm-define (doc-data-activate-here)
  (with-innermost dd 'doc-data
    (with-innermost t 'doc-inactive
      (tree-remove-node! t 0)
      (doc-data-go-to-active dd (tree-down-index dd)))))

(tm-define (doc-data-has-hidden?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (with fun (lambda (t) (or (tree-in? t (doc-title-inactive-tag-list))
				(tree-is? t 'doc-inactive)))
	(list-or (map fun l))))))

(tm-define (doc-data-deactivated?)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (list-or (map (lambda (t) (== (tm-car t) 'doc-inactive)) l)))))

(define (doc-data-activate-one t)
  (when (tree-is? t 'doc-inactive)
    (tree-remove-node! t 0)))

(tm-define (doc-data-activate-all)
  (with-innermost t 'doc-data
    (with i (tree-down-index t)
      (with l (cdr (tree->list t))
	(for-each doc-data-activate-one l))
      (doc-data-go-to-active t i))))

(define (doc-data-deactivate-one t)
  (if (in? (tm-car t) doc-data-inactive-tags)
      (tree-insert-node! t 0 '(doc-inactive))))

(tm-define (doc-data-deactivate-all)
  (with-innermost t 'doc-data
    (with l (cdr (tree->list t))
      (for-each doc-data-deactivate-one l))))

(tm-define (doc-data-activate-toggle)
  (if (doc-data-deactivated?)
      (doc-data-activate-all)
      (doc-data-deactivate-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making letter headings or titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-end-of-header-element)
  (if (inside? 'address) (go-end-of 'address))
  (if (inside? 'destination) (go-end-of 'destination))
  (if (inside? 'cc) (go-end-of 'cc))
  (if (inside? 'encl) (go-end-of 'encl))
  (go-end-line))

(tm-define (make-header l)
  (go-end-of-header-element)
  (if (!= (tree->stree (paragraph-tree)) "") (insert-return))
  (make l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sectional commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (section-context? t)
  (tree-in? t (numbered-unnumbered-append (section-tag-list))))

(tm-define (previous-section)
  (with bt (buffer-tree)
    (and (cursor-inside? bt)
	 (with bp (list-drop (cursor-path) (length (tree->path bt)))
	   (with sp (path-previous-section bt bp)
	     (and (!= sp bp) (path->tree (append (tree->path bt) sp))))))))

(define (selection-trim-ending)
  (if (selection-active-any?)
    (with st (selection-tree)
      (if (and (not  (tree-atomic? st ))
               (tree-empty? (tree-ref st :last)))             
        (begin 
          (selection-set 
            (selection-get-start) 
            (path-previous (root-tree) (selection-get-end)))
          (selection-trim-ending))))))

(define (make-section-aux l flag)
  (if (selection-active-any?) 
    (let 
      ((cp (cursor-path))
       (selstart (selection-get-start)))
      (selection-trim-ending)
      (if (tree-multi-paragraph? (selection-tree))
        (set-message "make-section error" "invalid multi-paragraph selection")
        (with selend  (selection-get-end)
          (make l)
          (if flag (make-return-before)
            (if (or (path-less-eq? cp selstart) (path-less? selend cp))
              ;; reposition cursor when its path still exists
              (go-to cp))))))
    (if (not (make-return-after))
      (begin 
        (make l)
        (if flag (make-return-before))))))
       
(tm-define (make-section l)
  (make-section-aux l #f))

(tm-define (make-unnamed-section l)
  (make-section-aux l #t))

(tm-define (kbd-enter t shift?)
  (:require (section-context? t))
  (tree-go-to t :end)
  (insert-return))

(tm-define (label-insert t)
  (:require (section-context? t))
  (tree-go-to t :end)
  (make 'label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for lists, enumerations and description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-context? t)
  (tree-in? t (list-tag-list)))

(tm-define (itemize-context? t)
  (tree-in? t (itemize-tag-list)))

(tm-define (enumerate-context? t)
  (tree-in? t (enumerate-tag-list)))

(tm-define (itemize-enumerate-context? t)
  (or (tree-in? t (itemize-tag-list))
      (tree-in? t (enumerate-tag-list))))

(tm-define (make-tmlist l)
  (make l)
  (make-item))

(tm-define (make-item)
  (if (not (make-return-after))
      (with lab (inside-which (list-tag-list))
	(cond ((in? lab (itemize-tag-list)) (make 'item))
	      ((in? lab (enumerate-tag-list)) (make 'item))
	      ((in? lab (description-tag-list)) (make 'item*))))))

(tm-define (kbd-enter t shift?)
  (:require (list-context? t))
  (if shift? (make-return-after) (make-item)))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'item*))
  (go-end-of 'item*))

(tm-define (numbered-context? t)
  (:require (or (itemize-context? t) (enumerate-context? t)))
  #t)

(tm-define (numbered-numbered? t)
  (:require (enumerate-context? t))
  #t)

(tm-define (numbered-toggle t)
  (:require (itemize-context? t))
  (variant-set t 'enumerate))

(tm-define (numbered-toggle t)
  (:require (enumerate-context? t))
  (variant-set t 'itemize))

(tm-define (standard-parameters l)
  (:require (== l "itemize"))
  (cons "itemize-levels" (search-parameters "itemize-1")))

(tm-define (standard-parameters l)
  (:require (== l "enumerate"))
  (cons "enumerate-levels" (search-parameters "enumerate-1")))

(tm-define (parameter-choice-list l)
  (:require (in? l (list "itemize-levels" "enumerate-levels")))
  (list "1" "2" "3" "4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hyphenation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (hyphenate-selection-as hyph)
  (:interactive #t)
  (:argument hyph "Hyphenate as")
  (:proposals hyph (list (tm->string (selection-tree))))
  (with ins `(hyphenate-as ,hyph ,(selection-tree))
    (clipboard-cut "null")
    (insert ins)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-equation)
  (make 'equation)
  (temp-proof-fix))

(tm-define (make-equation*)
  (make 'equation*)
  (temp-proof-fix))

(tm-define (make-eqnarray*)
  (make 'eqnarray*)
  (temp-proof-fix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-aux env var aux)
  (when (context-has? var)
    (set! aux (get-env var)))
  (if (not (make-return-after))
      (insert (list (string->symbol env) aux '(document "")))))

(tm-define (make-aux* env var aux name)
  (when (context-has? var)
    (set! aux (get-env var)))
  (if (not (make-return-after))
      (insert (list (string->symbol env) aux name '(document "")))))

(define (normalized-bib-name f)
  (if (string? f) f
      (let* ((r (url-delta (current-buffer) f))
             (n (if (== (url-suffix r) "bib") (url-unglue r 4) r)))
        (url->string n))))

(tm-define (make-bib file-name)
  (:argument file-name "Bibliography file")
  (let* ((name (normalized-bib-name file-name))
         (aux (if (context-has? "bib-prefix") (get-env "bib-prefix") "bib")))
    (if (not (make-return-after))
        (insert-go-to `(bibliography ,aux "tm-plain" ,name (document ""))
                      '(3 0 0)))))

(tm-define (make-database-bib)
  (with aux (if (context-has? "bib-prefix") (get-env "bib-prefix") "bib")
    (if (not (make-return-after))
        (insert-go-to `(bibliography ,aux "tm-plain" "" (document ""))
                      '(3 0 0)))))

(tm-define (automatic-section-context? t)
  (tree-in? t (automatic-section-tag-list)))

(define (automatic-name-var t)
  (cond ((tm-func? t 'table-of-contents) "table-of-contents-text")
        ((tm-func? t 'bibliography) "bibliography-text")
        ((tm-func? t 'the-index) "index-text")
        ((tm-func? t 'the-glossary) "glossary-text")
        ((tm-func? t 'list-of-figures) "list-of-figures")
        ((tm-func? t 'list-of-tables) "list-of-tables")
        (else #f)))

(define (automatic-section-name)
  (with-innermost t automatic-section-context?
    (let* ((var (automatic-name-var t))
           (val (if var (get-env-tree var) "")))
      (when (tm-func? val 'macro 1)
        (set! val (tm-ref val 0)))
      (when (and (tm-func? val 'localize 1) (tm-atomic? (tm-ref val 0)))
        (set! val (tm-ref val 0)))
      (if (tm-atomic? val) (tm->string val) ""))))

(tm-define (automatic-section-rename new-name)
  (:argument new-name "New name")
  (:proposals new-name (list (automatic-section-name)))
  (with-innermost t automatic-section-context?
    (when t
      (let* ((l (tree-label t))
             (l* (symbol-append l '*)))
        (tree-set t `(,l* ,@(cDr (tm-children t))
                          ,new-name ,(tm-ref t :last)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing enunciations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-category p)
  (:require (in? p (list "framed-theorems" "hanging-theorems")))
  :theorem-decorations)

(tm-define (dueto-supporting-context? t)
  (or (tree-in? t (numbered-unnumbered-append (enunciation-tag-list)))
      (tree-in? t (render-enunciation-tag-list))
      (tree-in? t '(proof render-proof))))

(tm-define (dueto-added? t)
  (tm-find t (lambda (x) (tm-is? x 'dueto))))

(tm-define (dueto-add t)
  (tree-go-to t :last :start)
  (make 'dueto))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (algorithm-context? t)
  (tree-in? t (algorithm-tag-list)))

(tm-define (algorithm-root s)
  (cond ((symbol-ends? s '*)
	 (algorithm-root (symbol-drop-right s 1)))
	((symbol-starts? s 'specified-)
	 (algorithm-root (symbol-drop s 10)))
	((symbol-starts? s 'named-)
	 (algorithm-root (symbol-drop s 6)))
	(else s)))

(tm-define (algorithm-numbered? t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (in? l (list r (symbol-append 'specified- r)))))

(tm-define (algorithm-named? t)
  (with l (tree-label t)
    (symbol-starts? l 'named-)))

(tm-define (algorithm-specified? t)
  (with l (tree-label t)
    (or (symbol-starts? l 'named-specified-)
	(symbol-starts? l 'specified-))))

(tm-define (algorithm-toggle-number t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (if (algorithm-numbered? t)
	(if (algorithm-specified? t)
	    (variant-set t (symbol-append 'specified- r '*))
	    (variant-set t (symbol-append r '*)))
	(if (algorithm-specified? t)
	    (variant-set t (symbol-append 'specified- r))
	    (variant-set t r)))))

(tm-define (algorithm-toggle-name t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (if (algorithm-named? t)
	(begin
	  (if (algorithm-specified? t)
	      (tree-assign-node! t (symbol-append 'specified- r))
	      (tree-assign-node! t r))
	  (tree-remove! t 0 1))
	(begin
	  (if (algorithm-specified? t)
	      (tree-assign-node! t (symbol-append 'named-specified- r))
	      (tree-assign-node! t (symbol-append 'named- r)))
	  (tree-insert! t 0 '(""))
	  (tree-go-to t 0 :start)))))

(tm-define (algorithm-toggle-specification t)
  (let* ((l (tree-label t))
	 (r (algorithm-root l)))
    (if (algorithm-specified? t)
	(begin
	  (cond ((algorithm-named? t)
		 (tree-assign-node! t (symbol-append 'named- r)))
		((algorithm-numbered? t)
		 (tree-assign-node! t r))
		(else
		 (tree-assign-node! t (symbol-append r '*))))
	  (tree-remove! t (- (tree-arity t) 2) 1))
	(begin
	  (cond ((algorithm-named? t)
		 (tree-assign-node! t (symbol-append 'named-specified- r)))
		((algorithm-numbered? t)
		 (tree-assign-node! t (symbol-append 'specified- r)))
		(else
		 (tree-assign-node! t (symbol-append 'specified- r '*))))
	  (tree-insert! t (- (tree-arity t) 1) '((document "")))
	  (tree-go-to t (- (tree-arity t) 2) :start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Possible to change the title of titled environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (titled-context? t)
  (tree-in? t (numbered-unnumbered-append (titled-tag-list))))

(tm-define (titled-named? t)
  (tree-in? t (render-titled-tag-list)))

(tm-define (titled-toggle-name t)
  (cond ((tree-in? t (numbered-unnumbered-append (theorem-tag-list)))
         (tree-set! t `(render-theorem "" ,(tree-ref t 0))))
        ((tree-in? t (numbered-unnumbered-append (remark-tag-list)))
         (tree-set! t `(render-remark "" ,(tree-ref t 0))))
        ((tree-in? t '(question))
         (tree-set! t `(render-theorem "" ,(tree-ref t 0))))
        ((tree-in? t '(answer))
         (tree-set! t `(render-remark "" ,(tree-ref t 0))))
        ((tree-in? t (numbered-unnumbered-append (exercise-tag-list)))
         (tree-set! t `(render-exercise "" ,(tree-ref t 0))))
        ((tree-in? t (numbered-unnumbered-append (solution-tag-list)))
         (tree-set! t `(render-solution "" ,(tree-ref t 0))))
        ((tree-in? t '(proof))
         (tree-set! t `(render-proof "" ,(tree-ref t 0))))
        ((tree-in? t (numbered-unnumbered-append (small-figure-tag-list)))
         (tree-set! t `(render-small-figure "" "" ,(tree-ref t 0)
                                                  ,(tree-ref t 1))))
        ((tree-in? t (numbered-unnumbered-append (big-figure-tag-list)))
         (tree-set! t `(render-big-figure "" "" ,(tree-ref t 0)
                                                ,(tree-ref t 1))))
        ((tree-is? t 'render-theorem)
         (tree-set! t `(theorem ,(tree-ref t 1))))
        ((tree-is? t 'render-remark)
         (tree-set! t `(remark ,(tree-ref t 1))))
        ((tree-is? t 'render-exercise)
         (tree-set! t `(exercise ,(tree-ref t 1))))
        ((tree-is? t 'render-solution)
         (tree-set! t `(solution ,(tree-ref t 1))))
        ((tree-is? t 'render-proof)
         (tree-set! t `(proof ,(tree-ref t 1))))
        ((tree-is? t 'render-small-figure)
         (tree-set! t `(small-figure ,(tree-ref t 2) ,(tree-ref t 3))))
        ((tree-is? t 'render-big-figure)
         (tree-set! t `(big-figure ,(tree-ref t 2) ,(tree-ref t 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Framed environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (frame-context? t)
  (tree-in? t (frame-tag-list)))

(tm-define (frame-titled-context? t)
  (tree-in? t (frame-titled-tag-list)))

(tm-define (frame-titled? t)
  (tree-in? t (frame-titled-tag-list)))

(tm-define (frame-toggle-title t)
  (cond ((tree-in? t (frame-tag-list))
         (with l (symbol-append (tree-label t) '-titled)
           (tree-set! t `(,l ,(tree-ref t 0) ""))
           (tree-go-to t 1 :end)))
        ((tree-in? t (frame-titled-tag-list))
         (with l (symbol-drop-right (tree-label t) 7)
           (tree-set! t `(,l ,(tree-ref t 0)))
           (tree-go-to t 0 :end)))))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(padded padded-titled)))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(overlined overlined-titled)))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")
        (list "overlined-sep" "Inner")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(underlined underlined-titled)))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")
        (list "underlined-sep" "Inner")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(bothlined bothlined-titled)))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")
        (list "overlined-sep" "Top")
        (list "underlined-sep" "Bottom")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(framed framed-titled)))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")
        (list "framed-vsep" "Inner")
        (list "framed-hsep" "Indentation")
        (list "framed-color" "Color")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'ornamented))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")
        (list "ornament-vpadding" "Inner")
        (list "ornament-hpadding" "Indentation")
        (list "ornament-color" "Color")
        (list "ornament-shape" "Shape")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'ornamented-titled))
  (list (list "padding-above" "Above")
        (list "padding-below" "Below")
        (list "ornament-vpadding" "Inner")
        (list "ornament-hpadding" "Indentation")
        (list "ornament-color" "Color")
        (list "ornament-shape" "Shape")
        (list "ornament-extra-color" "Title color")
        (list "ornament-title-style" "Title style")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'ornament))
  (append (list (list "ornament-shape" "Shape")
                (list "ornament-color" "Color"))
          (if (== (tree-arity t) 1) (list)
              (list (list "ornament-title-style" "Title style")
                    (list "ornament-extra-color" "Title color")))
          (list (list "ornament-border" "Border width")
                (list "ornament-hpadding" "Horizontal padding")
                (list "ornament-vpadding" "Vertical padding"))))

(tm-define (customizable-parameters t)
  (:require (tree-in? t (ornament-tag-list)))
  (list (list "ornament-shape" "Shape")
        (list "ornament-border" "Border width")
        (list "ornament-hpadding" "Horizontal padding")
        (list "ornament-vpadding" "Vertical padding")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Floating objects and environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (float-context? t)
  (tree-in? t '(float wide-float)))

(tm-define (footnote-context? t)
  (tree-in? t '(footnote wide-footnote)))

(tm-define (float-or-footnote-context? t)
  (tree-in? t '(float wide-float footnote wide-footnote)))

(tm-define (phantom-float-context? t)
  (tree-is? t 'phantom-float))

(tm-define (rich-float-context? t)
  (cond ((tree-in? t '(float wide-float)) #t)
        ((tree-in? t '(big-figure big-figure*
                       big-table big-table*
                       algorithm algorithm*
                       document concat))
         (and (tree-up t) (rich-float-context? (tree-up t))))
        (else #f)))

(tm-define (floatable-context? t)
  (and (tree-in? t '(big-figure big-figure*
                     big-table big-table*
                     algorithm algorithm*))
       (not (rich-float-context? t))
       (tree-is? t :up 'document)))

(tm-define (turn-floating t)
  (when (floatable-context? t)
    (with f `(float "float" "thb" ,t)
      (tree-set! t f)
      (tree-go-to t :start)
      (remove-text #f))))

(tm-define (float-wide? t)
  (and-with f (tree-search-upwards t float-or-footnote-context?)
    (tree-in? f '(wide-float wide-footnote))))

(tm-define (test-float-wide? . args)
  (float-wide? (focus-tree)))
(tm-define (float-toggle-wide t)
  (:check-mark "v" test-float-wide?)
  (and-with f (tree-search-upwards t float-or-footnote-context?)
    (cond ((tree-is? f 'float) (tree-assign-node f 'wide-float))
          ((tree-is? f 'wide-float) (tree-assign-node f 'float))
          ((tree-is? f 'footnote) (tree-assign-node f 'wide-footnote))
          ((tree-is? f 'wide-footnote) (tree-assign-node f 'footnote)))))

(tm-define (cursor-at-anchor?)
  (with t (cursor-tree)
    (float-or-footnote-context? t)))

(tm-define (go-to-anchor)
  (cond ((or (inside? 'float) (inside? 'wide-float))
         (with-innermost t float-context?
           (tree-go-to t :end)))
        ((or (inside? 'footnote) (inside? 'wide-footnote))
         (with-innermost t footnote-context?
           (tree-go-to t :end)))))

(tm-define (go-to-float)
  (with t (cursor-tree)
    (cond ((float-context? t)
           (tree-go-to t 2 :start))
          ((footnote-context? t)
           (tree-go-to t 0 :start)))))

(tm-define (cursor-toggle-anchor)
  (:check-mark "v" cursor-at-anchor?)
  (if (cursor-at-anchor?)
      (go-to-float)
      (go-to-anchor)))
