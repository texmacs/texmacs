
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-widgets.scm
;; DESCRIPTION : widgets for editing macros
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-widgets)
  (:use (source macro-edit)
	(version version-edit) ;; FIXME: for selection-trees
	(generic format-edit)
        (generic document-part)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major operation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define macro-major-mode :global)
(tm-define macro-major-focus #f)
(tm-define macro-major-history (list))

(define (initialize-macro-editor l mode)
  (terminate-macro-editor)
  (set! macro-major-mode mode)
  (set! macro-major-history (list))
  (when (func? mode :local)
    (set! macro-major-focus (tree->tree-pointer (focus-tree)))))

(define (terminate-macro-editor)
  (when macro-major-focus
    (tree-pointer-detach macro-major-focus)
    (set! macro-major-focus #f)))

(define (macro-editor-get l)
  (cond ((== macro-major-mode :global)
         (get-definition l))
        ((func? macro-major-mode :local)
         (and-with t (tree-pointer->tree macro-major-focus)
           (with val (tree-with-get t l)
             (if val (tm->tree `(assign ,l ,val)) (get-definition l)))))
        (else #f)))

(define (macro-editor-set l val u)
  (when (symbol? l) (set! l (symbol->string l)))
  (cond ((== macro-major-mode :global)
         (macro-set-value l val u))
        ((func? macro-major-mode :local)
         (and-with t (tree-pointer->tree macro-major-focus)
           (tree-with-set t l val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define macro-current-macro  "")
(tm-define macro-current-filter "")
(tm-define macro-current-mode "Text")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for macro editing widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macro-retrieve* u)
  (and-with t (buffer-get-body u)
    (if (tm-is? t 'document) (set! t (tm-ref t :last)))
    (and (tm-in? t '(edit-macro edit-tag)) t)))

(define (macro-retrieve u)
  (and-with t (macro-retrieve* u)
    (cond ((tm-is? (tree-ref t :last) 'inactive*)
           `(,(tm-label t) ,@(cDr (tm-children t)) ,(tree-ref t :last 0)))
          (else t))))

(define (macro-retrieve-name u)
  (and-with t (macro-retrieve u)
    (tree->string (tm-ref t 0))))

(define (get-macro-mode)
  macro-current-mode)

(define (set-macro-mode u mode)
  (set! macro-current-mode mode)
  (and-with t (macro-retrieve u)
    (with t* (macro-retrieve* u)
      (cond ((== mode "Source")
             (tree-set t* :last `(inactive* ,(cAr (tm-children t)))))
            (else
              (tree-set t* :last (cAr (tm-children t)))))
      (refresh-now "macro-editor-mode"))))

(tm-define (toggle-source-mode)
  (:require (has-style-package? "macro-editor"))
  (with mode (if (== (get-macro-mode) "Text") "Source" "Text")
    (set-macro-mode (current-buffer) mode)))

(define (preamble-insert pre ass)
  (with m (list-find (reverse (tree-children pre))
                     (lambda (x)
                       (and (tree-is? x 'assign)
                            (tm-equal? (tm-ref x 0) (tm-ref ass 0)))))
    (if m
        (tree-set m ass)
        (tree-insert pre (tree-arity pre) (list ass)))))

(define (macro-set-value l mac u)
  (let* ((b   (buffer-get-master u))
         (m   (buffer-get-master b))
         (buf (buffer-get-body b))
         (old (get-definition* l buf))
         (new `(assign ,l ,mac)))
    (cond ((or (not (buffer-exists? u)) (not (buffer-exists? b))) #f)
          ((and old (tree->path old)) (tree-set old 1 mac))
          (else
            (when (not (document-has-preamble? buf))
              (tree-insert! buf 0 '((hide-preamble (document "")))))
            (when (document-has-preamble? buf)
              (with pre (tree-ref buf 0 0)
                (preamble-insert pre new)))
            (when (!= m b)
              (macro-set-value l mac b))))))

(define (macro-value t)
  (if (tm-is? t 'edit-macro)
      `(macro ,@(cdr (tm-children t)))
      (tm-ref t 1)))

(define (macro-apply u)
  (and-with t (macro-retrieve u)
    (macro-editor-set (tree->string (tm-ref t 0)) (macro-value t) u)))

(define (build-macro-document* l def)
  (when (and (tm-func? def 'assign 2)
	     (tm-equal? (tm-ref def 1) '(uninit)))
    (set! def `(assign ,(tm-ref def 0) (macro ""))))
  (let* ((mac (if (tm-func? (tm-ref def 1) 'macro)
		  `(edit-macro ,l ,@(tm-children (tm-ref def 1)))
		  `(edit-tag ,l ,(tm-ref def 1))))
         (mac* (if (!= macro-current-mode "Source") mac
                   `(,@(cDr mac) (inactive* ,(cAr mac)))))
	 (pre (document-get-preamble (buffer-tree)))
	 (doc `(document (hide-preamble ,pre) ,mac*)))
    doc))

(define (build-macro-document l)
  (and-with def (macro-editor-get l)
    (build-macro-document* l def)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing a single macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((macro-editor u packs doc mode) quit)
  (padded
    (resize "600px" "300px"
      (texmacs-input doc `(style (tuple ,@packs)) u))
    ===
    (hlist
      (refreshable "macro-editor-mode"
        (enum (set-macro-mode u answer)
              '("Text" "Source")
              (get-macro-mode) "6em"))
      >>
      (explicit-buttons
        ("Apply" (macro-apply u))
        // //
        ("Ok" (macro-apply u) (quit))))))

(tm-define (editable-macro? l)
  (if (symbol? l) (set! l (symbol->string l)))
  (and (tree-label-extension? (string->symbol l))
       (nin? l (list "edit-macro" "edit-tag"))
       (get-definition l)))

(tm-define (open-macro-editor l mode)
  (:interactive #t)
  (if (symbol? l) (set! l (symbol->string l)))
  (initialize-macro-editor l mode)
  (let* ((b (current-buffer-url))
         (u (string->url (string-append "tmfs://aux/edit-" l)))
         (packs (get-style-list))
         (styps (list-remove-duplicates (append packs (list "macro-editor")))))
    (and-with doc (build-macro-document l)
      (dialogue-window (macro-editor u styps doc "Text")
                       (lambda x (terminate-macro-editor))
                       "Macro editor" u)
      (buffer-set-master u b))))

(tm-define (edit-focus-macro)
  (:interactive #t)
  (with l (symbol->string (macro-label (focus-tree)))
    (when (editable-macro? l)
      (if (has-style-package? "macro-editor")
          (with old (macro-retrieve-name (current-buffer))
            (macros-editor-select (current-buffer) l "")
            (set! macro-major-history (cons old macro-major-history)))
          (open-macro-editor l :global)))))

(tm-define (edit-previous-macro)
  (when (and (nnull? macro-major-history)
             (has-style-package? "macro-editor"))
    (macros-editor-select (current-buffer) (car macro-major-history) "")
    (set! macro-major-history (cdr macro-major-history))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (contains-table? t)
  (or (and (tm-func? t 'table) t)
      (and (tm-compound? t)
	   (list-or (map contains-table? (tm-children t))))))

(tm-define (can-create-table-macro?)
  (or (inside? 'table)
      (and (selection-active-any?)
	   (list-or (map contains-table? (selection-trees))))))

(define (position-inside-table)
  (or (inside? 'table)
      (and-with t (can-create-table-macro?)
	(while (tm-in? t '(tformat table row cell))
	  (set! t (tm-ref t 0)))
	(tree-go-to t :start))))

(define (tformat-subst-selection t tf)
  (cond ((tm-atomic? t) t)
	((tm-func? t 'tformat)
	 (with r (tformat-subst-selection (cAr (tm-children t)) tf)
	   (if (tm-func? r 'tformat) r
	       (append (cDr (tm-children t)) (list r)))))
	((tm-in? t '(table tabular tabular* wide-tabular
			   block block* wide-block)) tf)
	(else (cons (tm-label t)
		    (map (cut tformat-subst-selection <> tf)
			 (tm-children t))))))

(tm-define (create-table-macro l mode)
  (:interactive #t)
  (when (can-create-table-macro?)
    (position-inside-table)
    (if (symbol? l) (set! l (symbol->string l)))
    (let* ((b (current-buffer-url))
	   (u (string-append "tmfs://aux/edit-" l))
	   (packs (get-style-list))
	   (styps (list-remove-duplicates
		   (append packs (list "macro-editor"))))
	   (fm (table-get-format-all))
	   (tf `(tformat ,@(tree-children fm) (arg "body")))
	   (body (if (selection-active-any?)
		     (with sel (tm->stree (selection-tree))
		       (tformat-subst-selection sel tf))
		     tf))
	   (def `(assign ,l (inactive* (macro "body" ,body)))))
      (and-with doc (build-macro-document* l def)
	(dialogue-window (macro-editor u styps doc "Source")
			 (lambda x (noop))
			 "Macro editor")
	(buffer-set-master u b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing a macro chosen from the list of all defined macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (macros-editor-select u macro filter)
  (set! macro-current-macro  macro)
  (set! macro-current-filter filter)
  (tree-set (buffer-get-body u)
            (build-macro-document macro-current-macro))
  (refresh-now "macros-editor-documentation"))

(tm-define (macros-editor-current-help)
  (with doc (tmdoc-search-tag (string->symbol macro-current-macro))
    (if doc (tm->stree doc)
        `(document (em "No documentation available.")))))

(tm-widget ((macros-editor u packs l) quit)
  (padded
    (horizontal
      (vertical
        (bold (text "Macro name"))
        === ===
        (resize "250px" "500px"
          (filtered-choice (macros-editor-select u answer filter) l
                           macro-current-macro macro-current-filter)))
      ///
      (vertical
        (bold (text "Macro definition"))
        === ===
        (resize "500px" "220px"
          (texmacs-input (build-macro-document macro-current-macro)
                         `(style (tuple ,@packs)) u))
        ===
        (glue #f #t 0 10)
        ===
        (bold (text "Documentation"))
        === ===
        (horizontal
          (glue #t #f 0 0)
          (resize "500px" "220px"
            (refreshable "macros-editor-documentation"
              (texmacs-output
               `(document
                  (mini-paragraph "480px" ,(macros-editor-current-help)))
               '(style "tmdoc"))))
          (glue #t #f 0 0))))
    ======
    (hlist
      (refreshable "macro-editor-mode"
        (enum (set-macro-mode u answer)
              '("Text" "Source")
              (get-macro-mode) "6em"))
      >>
      (explicit-buttons
        ("Apply" (macro-apply u))
	// //
	("Ok" (macro-apply u) (quit))))))

(define (get-key key-val)
  (tree->string (tree-ref key-val 0)))

(tm-define (all-defined-macros)
  (with env (tm-children (get-full-env))
    (sort (list-difference (map get-key env)
                           (list "atom-decorations" "line-decorations"
                                 "page-decorations" "xoff-decorations"
                                 "yoff-decorations"
                                 "cell-decoration" "cell-format"
                                 "wide-framed-colored"
                                 "wide-std-framed-colored"))
          string<=?)))

(tm-define (open-macros-editor mode)
  (:interactive #t)
  (initialize-macro-editor :all mode)
  (let* ((b (current-buffer-url))
	 (u (string->url "tmfs://aux/macro-editor"))
	 (names (all-defined-macros))
         (packs (get-style-list))
         (styps (list-remove-duplicates (append packs (list "macro-editor")))))
    (dialogue-window (macros-editor u styps names)
		     (lambda x (terminate-macro-editor))
		     "Macros editor" u)
    (buffer-set-master u b)))
