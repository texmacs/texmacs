
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-menu.scm
;; DESCRIPTION : default focus menu
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-menu)
  (:use (utils edit variants)
	(utils edit selections)
	(generic generic-edit)
	(generic format-edit)
	(generic format-geometry-edit)
        (generic document-edit)
        (source source-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-variants-of t)
  (variants-of (tree-label t)))

(tm-define (focus-tag-name l)
  (let* ((s (symbol->string l))
         (th (member->theme s)))
    (if th
        (with ns (string-drop s (+ (string-length th) 1))
          (focus-tag-name (string->symbol ns)))
        (if (symbol-unnumbered? l)
            (focus-tag-name (symbol-drop-right l 1))
            (with r (upcase-first (tree-name (tree l)))
              (string-replace r "-" " "))))))

(tm-menu (focus-variant-menu t)
  (for (v (focus-variants-of t))
    ((eval (focus-tag-name v))
     (variant-set-keep-numbering (focus-tree) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for hidden fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string-variable-name? t i)
  (and (== (tree-child-type t i) "variable")
       (tree-in? t '(with attr style-with style-with*))
       (tree-atomic? (tree-ref t i))
       (!= (tree->stree (tree-ref t i)) "")))

(tm-define (hidden-child? t i)
  (and (not (tree-accessible-child? t i))
       (not (string-variable-name? t i))
       (!= (type->format (tree-child-type t i)) "n.a.")))

(tm-define (child-proposals t i)
  #f)

(define (hidden-children t)
  (with fun (lambda (i) (if (hidden-child? t i) (list (tree-ref t i)) (list)))
    (append-map fun (.. 0 (tree-arity t)))))

(define (tree-child-name* t i)
  (with s (tree-child-name t i)
    (cond ((!= s "") s)
          ((and (> i 0) (string-variable-name? t (- i 1)))
           (with r (tree->string (tree-ref t (- i 1)))
             (string-replace r "-" " ")))
          ((> (length (hidden-children t)) 1) "")
          ((== (tree-child-type t i) "regular") "")
          (else (tree-child-type t i)))))

(define (tree-child-long-name* t i)
  (with s (tree-child-long-name t i)
    (cond ((!= s "") s)
          ((and (> i 0) (string-variable-name? t (- i 1)))
           (with r (tree->string (tree-ref t (- i 1)))
             (string-replace r "-" " ")))
          ((> (length (hidden-children t)) 1) "")
          ((== (tree-child-type t i) "regular") "")
          (else (tree-child-type t i)))))

(define (type->format type)
  (cond ((== type "adhoc") "n.a.")
        ((== type "raw") "n.a.")
        ((== type "url")
         ;; FIXME: filename editing is way too slow in Qt and
         ;; tab completion does not seem to work anyway
         (if (qt-gui?) "string" "smart-file"))
        ((== type "graphical") "n.a.")
        ((== type "point") "n.a.")
        ((== type "obsolete") "n.a.")
        ((== type "unknown") "n.a.")
        ((== type "error") "n.a.")
        (else "string")))

(define (type->width type)
  (cond ((== type "boolean") "5em")
        ((== type "integer") "5em")
        ((== type "length") "5em")
        ((== type "numeric") "5em")
        ((== type "identifier") "8em")
        ((== type "duration") "5em")
        (else "1w")))

(tm-define (inputter-active? t type)
  (cond ((== type "length") (tm-rich-length? t))
	(else (tree-atomic? t))))

(tm-define (inputter-decode t type)
  (cond ((== type "length") (tm->rich-length t))
	(else (tree->string t))))

(tm-define (inputter-encode s type)
  (cond ((== type "length") (rich-length->tm s))
	(else s)))

(tm-menu (string-input-icon t i)
  (let* ((name (tree-child-name* t i))
         (type (tree-child-type t i))
         (s (string-append (upcase-first name) ":"))
         (active? (inputter-active? (tree-ref t i) type))
         (props (child-proposals t i))
	 (in (if active? (inputter-decode (tree-ref t i) type) "n.a."))
         (in* (if active? in ""))
         (ins (if props (cons in props) (list in)))
         (fm (type->format type))
         (w (type->width type))
         (setter (lambda (x)
		   (when x
                     (tree-set (focus-tree) i (inputter-encode x type))
                     (focus-tree-modified (focus-tree))))))
    (assuming (== name "")
      //)
    (assuming (!= name "")
      (glue #f #f 3 0)
      (mini #t
        (if (and (!= type "color") props)
            (=> (eval s)
                (for (prop props)
                  ((eval prop) (setter prop)))))
        (if (or (== type "color") (not props))
            (group (eval s)))))
    (if (!= type "color")
        (when active?
          (mini #t
            (input (setter answer) fm ins w))))
    (if (== type "color")
        (=> (color (tree->stree (tree-ref t i)) #f #f 24 16)
            (pick-background "" (setter answer))
            ---
            ("Palette" (interactive-color setter '()))
            ("Pattern" (open-pattern-selector setter "1cm"))
            ("Picture" (open-background-picture-selector setter))
            ("Other" (interactive setter
                       (list (upcase-first name) "color" in*)))))))

(tm-menu (string-input-menu t i)
  (let* ((name (tree-child-long-name* t i))
         (s `(concat "Set " ,name))
         (prompt (upcase-first name))
         (type (tree-child-type t i))
         (fm (type->format type))
         (setter (lambda (x)
		   (when x
		     (tree-set (focus-tree) i (inputter-encode x type))
                     (focus-tree-modified (focus-tree))))))
    (assuming (!= name "")
      (when (inputter-active? (tree-ref t i) type)
        ((eval s)
         (interactive setter
	   (list prompt fm (inputter-decode (tree-ref t i) type))))))))

(tm-menu (string-input-icon t i)
  (:require (string-variable-name? t i))
  (with c (tree-ref t i)
    (with s (if (tree-atomic? c) (tree->string c) "n.a.")
      (glue #f #f 3 0)
      (mini #t (group (eval (string-append s ":")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parameter-name l)
  (focus-tag-name (string->symbol (tree-name (list (string->symbol l))))))

(tm-menu (focus-parameter-menu-item l)
  ((eval (parameter-name l)) (open-macro-editor l)))

(tm-menu (init-env-menu l cs)
  (with ss (list-filter cs string?)
    ((check "Default" "*" (test-default? l))
     (init-default l))
    (if (nnull? ss)
        ---
        (for (c ss)
          (if (string? c)
              ((check (eval (upcase-first c)) "*" (test-init? l c))
               (set-init-env l c)))))
    (if (and (nnull? ss) (in? :other cs))
        ---)
    (if (in? :other cs)
        ("Other" (init-interactive-env l)))))

(tm-menu (focus-parameter-menu-item l)
  (:require (and (tree-label-parameter? (string->symbol l))
                 (string? (get-init-env l))
                 (nin? (tree-label-type (string->symbol l))
                       (list "unknown" "regular" "adhoc"))))
  (-> (eval (focus-tag-name (string->symbol l)))
      (dynamic (init-env-menu l (list :other)))))

(tm-menu (focus-parameter-menu-item l)
  (:require (and (tree-label-parameter? (string->symbol l))
                 (string? (get-init-env l))
                 (== (tree-label-type (string->symbol l)) "boolean")))
  ((check (eval (focus-tag-name (string->symbol l))) "v"
          (== (get-init-env l) "true"))
   (toggle-init-env l)))

(tm-menu (focus-parameter-menu-item l)
  (:require (and (tree-label-parameter? (string->symbol l))
                 (== (tree-label-type (string->symbol l)) "color")))
  (-> (eval (focus-tag-name (string->symbol l)))
      (with setter (lambda (col) (init-env-tree l col))
        ((check "Default" "*" (test-default? l)) (init-default l))
        ---
        (pick-background "" (setter answer))
        ---
        (if (in? l (list "locus-color" "visited-color"))
            ((check "Preserve" "*" (test-init? l "preserve"))
             (set-init-env l "preserve")))
        ("Palette" (interactive-color setter '()))
        ("Pattern" (open-pattern-selector setter "1cm"))
        ("Picture" (open-background-picture-selector setter))
        ("Other" (init-interactive-env l)))))

(tm-menu (focus-parameter-menu-item l)
  (:require (and (tree-label-parameter? (string->symbol l))
                 ;;(== (tree-label-type (string->symbol l)) "font")
                 (string-ends? l "-font")))
  (-> (eval (focus-tag-name (string->symbol l)))
      ((check "Default" "*" (test-default? l)) (init-default l))
      ---
      ((check "Roman" "*" (test-init? l "roman")) (init-env l "roman"))
      ((check "Stix" "*" (test-init? l "stix")) (init-env l "stix"))
      ((check "Bonum" "*" (test-init? l "bonum")) (init-env l "bonum"))
      ((check "Pagella" "*" (test-init? l "pagella")) (init-env l "pagella"))
      ((check "Schola" "*" (test-init? l "schola")) (init-env l "schola"))
      ((check "Termes" "*" (test-init? l "termes")) (init-env l "termes"))
      ---
      (with prefix (string-drop-right l 4)
        ("Other" (open-document-other-font-selector prefix)))))

(tm-menu (focus-parameter-menu-item l)
  (:require (and (tree-label-parameter? (string->symbol l))
                 (== (tree-label-type (string->symbol l)) "font-size")))
  (-> (eval (focus-tag-name (string->symbol l)))
      ((check "Default" "*" (test-default? l)) (init-default l))
      ---
      ((check "Small" "*" (test-init? l "0.841")) (init-env l "0.841"))
      ((check "Normal" "*" (test-init? l "1")) (init-env l "1"))
      ((check "Large" "*" (test-init? l "1.189")) (init-env l "1.189"))
      ((check "Very large" "*" (test-init? l "1.414")) (init-env l "1.414"))
      ((check "Huge" "*" (test-init? l "1.682")) (init-env l "1.682"))
      ---
      ("Other" (init-interactive-env l))))

(tm-menu (focus-parameter-menu-item l)
  (:require (parameter-choice-list l))
  (with cs (parameter-choice-list l)
    (-> (eval (focus-tag-name (string->symbol l)))
        (dynamic (init-env-menu l cs)))))

(tm-define (parameter-show-in-menu? l)
  (not (member->theme l)))

(tm-menu (focus-parameters-menu t)
  (with ps (list-filter (search-tag-parameters t) parameter-show-in-menu?)
    (if (nnull? ps)
        (group "Style parameters")
        (for (p ps)
          (dynamic (focus-parameter-menu-item p)))
        (if (tree-label-extension? (tree-label t))
            ---))))

(tm-menu (focus-theme-parameters-submenu th)
  (with mems (theme->members th)
    (for (mem mems)
      (with var (string-append th "-" mem)
        (dynamic (focus-parameter-menu-item var))))))

(tm-menu (focus-theme-parameters-menu t)
  (with ths (search-tag-themes t)
    (if (nnull? ths)
        (group "Theme parameters")
        (for (th ths)
          (-> (eval th)
              (dynamic (focus-theme-parameters-submenu th))))
        ---)))

(tm-define (parameter-show-in-menu? l)
  (:require (in? l (list "the-label" "auto-nr" "current-part" "language"
                         "page-nr" "page-the-page" "prog-language"
			 "caption-summarized" "figure-width")))
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Focus menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: when recovering focus-tree,
;; double check that focus-tree still has the required form

(tm-menu (focus-ancestor-menu t))

(tm-menu (focus-toggle-menu t)
  (assuming (numbered-context? t)
    ;; FIXME: itemize, enumerate, eqnarray*
    ((check "Numbered" "v" (numbered-numbered? (focus-tree)))
     (numbered-toggle (focus-tree))))
  (assuming (alternate-context? t)
    ((check (eval (alternate-second-name t)) "v"
            (alternate-second? (focus-tree)))
     (alternate-toggle (focus-tree))))
  (assuming (!= (tree-children t) (tree-accessible-children t))
    ((check "Show hidden" "v" (tree-is? t :up 'inactive))
     (inactive-toggle t))))

(tm-menu (focus-float-menu t))
(tm-menu (focus-animate-menu t))
(tm-menu (focus-misc-menu t))

(tm-menu (focus-style-options-menu t)
  (with opts (search-tag-options t)
    (if (nnull? opts)
        (group "Style options")
        (for (opt opts)
          ((check (balloon (eval (style-get-menu-name opt))
                           (eval (style-get-documentation opt))) "v"
                  (has-style-package? opt))
           (toggle-style-package opt)))
        (if (tree-label-extension? (tree-label t))
            ---))))

(tm-menu (focus-tag-edit-menu l)
  (if (tree-label-extension? l)
      (when (editable-macro? l)
        ("Edit macro" (open-macro-editor l)))
      (when (has-macro-source? l)
        ("Edit source" (edit-macro-source l)))))

(tm-menu (focus-preferences-menu t)
  (dynamic (focus-style-options-menu t))
  (dynamic (focus-parameters-menu t))
  (dynamic (focus-theme-parameters-menu t))
  (dynamic (focus-tag-edit-menu (tree-label t))))

(tm-menu (focus-theme-menu t))

(tm-menu (focus-tag-menu t)
  (with l (focus-variants-of t)
    (assuming (<= (length l) 1)
      (inert ((eval (focus-tag-name (tree-label t))) (noop) (noop))))
    (assuming (> (length l) 1)
      (-> (eval (focus-tag-name (tree-label t)))
          (dynamic (focus-variant-menu t)))))
  (dynamic (focus-toggle-menu t))
  (dynamic (focus-float-menu t))
  (dynamic (focus-animate-menu t))
  (dynamic (focus-misc-menu t))
  (assuming (focus-has-preferences? t)
    (-> "Preferences"
        (dynamic (focus-preferences-menu t))))
  (assuming (focus-has-theme? t)
    (-> "Rendering"
        (dynamic (focus-theme-menu t))))
  ("Describe" (focus-help))
  (assuming (focus-can-search? t)
    ("Search in database" (focus-open-search-tool t)))
  ("Delete" (remove-structure-upwards)))

(tm-menu (focus-move-menu t)
  ("Previous similar" (traverse-previous))
  ("Next similar" (traverse-next))
  ("First similar" (traverse-first))
  ("Last similar" (traverse-last))
  (assuming (cursor-inside? t)
    ("Exit left" (structured-exit-left))
    ("Exit right" (structured-exit-right))))

(tm-menu (focus-insert-menu t)
  (assuming (and (structured-horizontal? t) (not (structured-vertical? t)))
    (when (focus-can-insert? t)
      ("Insert argument before" (structured-insert-left))
      ("Insert argument after" (structured-insert-right)))
    (when (focus-can-remove? t)
      ("Remove argument before" (structured-remove-left))
      ("Remove argument after" (structured-remove-right))))
  (assuming (structured-vertical? t)
    ("Insert above" (structured-insert-up))
    ("Insert left" (structured-insert-left))
    ("Insert right" (structured-insert-right))
    ("Insert below" (structured-insert-down))
    ("Remove upwards" (structured-remove-up))
    ("Remove leftwards" (structured-remove-left))
    ("Remove rightwards" (structured-remove-right))
    ("Remove downwards" (structured-remove-down))))

(tm-menu (focus-extra-menu t))

(tm-define (hidden-inputter-children t)
  (append-map (lambda (c)
		(if (and-with i (tree-index c)
		      (with type (tree-child-type t i)
			(inputter-active? c type)))
		    (list c)
		    (list)))
              (hidden-children t)))

(tm-menu (focus-hidden-menu t)
  (assuming (nnull? (hidden-inputter-children t))
    ---
    (for (i (.. 0 (tree-arity t)))
      (assuming (hidden-child? t i)
        (dynamic (string-input-menu t i))))))

(tm-menu (focus-hidden-menu t)
  (:require (pure-alternate-context? t)))

(tm-menu (standard-focus-menu t)
  (dynamic (focus-ancestor-menu t))
  (dynamic (focus-tag-menu t))
  (assuming (focus-can-move? t)
    ---
    (dynamic (focus-move-menu t)))
  (assuming (focus-can-insert-remove? t)
    ---
    (dynamic (focus-insert-menu t)))
  (dynamic (focus-extra-menu t))
  (dynamic (focus-hidden-menu t)))

(tm-menu (focus-menu)
  (dynamic (standard-focus-menu (focus-tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main focus icons bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-ancestor-icons t))

(tm-menu (focus-toggle-icons t)
  (assuming (numbered-context? t)
    ((check (balloon (icon "tm_numbered.xpm") "Toggle numbering") "v"
            (numbered-numbered? (focus-tree)))
     (numbered-toggle (focus-tree))))
  (assuming (alternate-first? t)
    ((check (balloon (icon (eval (alternate-first-icon t)))
                     (eval (alternate-second-name t))) "v" #f)
     (alternate-toggle (focus-tree))))
  (assuming (alternate-second? t)
    ((check (balloon (icon (eval (alternate-second-icon t)))
                     (eval (alternate-second-name t))) "v" #t)
     (alternate-toggle (focus-tree))))
  (assuming (!= (tree-children t) (tree-accessible-children t))
    ((check (balloon (icon "tm_show_hidden.xpm") "Show hidden") "v"
            (tree-is? t :up 'inactive))
     (inactive-toggle t))))

(tm-menu (focus-float-icons t))
(tm-menu (focus-animate-icons t))
(tm-menu (focus-misc-icons t))
(tm-menu (focus-tag-extra-icons t))

(tm-menu (focus-tag-icons t)
  (dynamic (focus-toggle-icons t))
  (dynamic (focus-float-icons t))
  (dynamic (focus-animate-icons t))
  (dynamic (focus-misc-icons t))
  (mini #t
    (with l (focus-variants-of t)
      (assuming (<= (length l) 1)
        (inert ((eval (focus-tag-name (tree-label t))) (noop))))
      (assuming (> (length l) 1)
        (=> (balloon (eval (focus-tag-name (tree-label t)))
                     "Structured variant")
            (dynamic (focus-variant-menu t))))))
  (dynamic (focus-tag-extra-icons t))
  (assuming (cursor-inside? t)
    ((balloon (icon "tm_exit_left.xpm") "Exit tag on the left")
     (structured-exit-left))
    ((balloon (icon "tm_exit_right.xpm") "Exit tag on the right")
     (structured-exit-right))
    ((balloon (icon "tm_focus_delete.xpm") "Remove tag")
     (remove-structure-upwards)))
  (assuming (focus-has-preferences? t)
    (=> (balloon (icon "tm_focus_prefs.xpm") "Preferences for tag")
	(dynamic (focus-preferences-menu t))))
  (assuming (focus-has-theme? t)
    (=> (balloon (icon "tm_theme.xpm") "Rendering options for tag")
        (dynamic (focus-theme-menu t))))
  ((balloon (icon "tm_focus_help.xpm") "Describe tag")
   (focus-help))
  (assuming (focus-can-search? t)
    ((balloon (icon "tm_focus_search.xpm") "Search in database")
     (focus-open-search-tool t))))

(tm-menu (focus-move-icons t)
  ((balloon (icon "tm_similar_first.xpm") "Go to first similar tag")
   (traverse-first))
  ((balloon (icon "tm_similar_previous.xpm") "Go to previous similar tag")
   (traverse-previous))
  ((balloon (icon "tm_similar_next.xpm") "Go to next similar tag")
   (traverse-next))
  ((balloon (icon "tm_similar_last.xpm") "Go to last similar tag")
   (traverse-last)))

(tm-menu (focus-insert-icons t)
  (assuming (and (structured-horizontal? t) (not (structured-vertical? t)))
    (when (focus-can-insert? t)
      ((balloon (icon "tm_insert_left.xpm") "Structured insert at the left")
       (structured-insert-left))
      ((balloon (icon "tm_insert_right.xpm") "Structured insert at the right")
       (structured-insert-right)))
    (when (focus-can-remove? t)
      ((balloon (icon "tm_delete_left.xpm") "Structured remove leftwards")
       (structured-remove-left))
      ((balloon (icon "tm_delete_right.xpm") "Structured remove rightwards")
       (structured-remove-right))))
  (assuming (structured-vertical? t)
    ((balloon (icon "tm_insert_up.xpm") "Structured insert above")
     (structured-insert-up))
    ((balloon (icon "tm_insert_left.xpm") "Structured insert at the left")
     (structured-insert-left))
    ((balloon (icon "tm_insert_right.xpm") "Structured insert at the right")
     (structured-insert-right))
    ((balloon (icon "tm_insert_down.xpm") "Structured insert below")
     (structured-insert-down))
    ((balloon (icon "tm_delete_up.xpm") "Structured remove upwards")
     (structured-remove-up))
    ((balloon (icon "tm_delete_left.xpm") "Structured remove leftwards")
     (structured-remove-left))
    ((balloon (icon "tm_delete_right.xpm") "Structured remove rightwards")
     (structured-remove-right))
    ((balloon (icon "tm_delete_down.xpm") "Structured remove downwards")
     (structured-remove-down))))

(tm-menu (focus-extra-icons t))

(tm-menu (focus-hidden-icons t)
  (for (i (.. 0 (tree-arity t)))
    (assuming (hidden-child? t i)
      (dynamic (string-input-icon t i)))))

(tm-menu (focus-hidden-icons t)
  (:require (pure-alternate-context? t)))

(tm-menu (standard-focus-icons t)
  (dynamic (focus-ancestor-icons t))
  (assuming (focus-can-move? t)
    (minibar (dynamic (focus-move-icons t)))
    //)
  (assuming (focus-can-insert-remove? t)
    (minibar (dynamic (focus-insert-icons t)))
    //)
  (minibar (dynamic (focus-tag-icons t)))
  (dynamic (focus-extra-icons t))
  (dynamic (focus-hidden-icons t))
  //)

(tm-menu (texmacs-focus-icons)
  (assuming (in-graphics?)
    (dynamic (graphics-focus-icons)))
  (assuming (not (in-graphics?))
    (dynamic (standard-focus-icons (focus-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for customizable environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-customizable-menu-item setter var name)
  ((eval name) (interactive setter (list name "string" (get-env var)))))

(tm-menu (focus-customizable-menu-item setter var name)
  (:require (parameter-choice-list var))
  (-> (eval name)
      (for (val (parameter-choice-list var))
        ((eval val) (setter val)))))

(tm-menu (focus-customizable-menu-item setter var name)
  (:require (== (tree-label-type (string->symbol var)) "color"))
  (-> (eval name)
      (pick-background "" (setter answer))
      ---
      ("Palette" (interactive-color setter '()))
      ("Pattern" (open-pattern-selector setter "1cm"))
      ("Picture" (open-background-picture-selector setter))
      ("Other" (interactive setter (list name "string" (get-env var))))))

(tm-menu (focus-extra-menu t)
  (:require (customizable-context? t))
  ---
  (for (p (customizable-parameters t))
    (with (var name) p
      (with l (tree-label t)
        (with setter (lambda (val)
                       (when (tree-is? (focus-tree) l)
                         (tree-with-set (focus-tree) var val)))
          (dynamic (focus-customizable-menu-item setter var name)))))))

(tm-menu (focus-customizable-icons-item setter var name)
  (input (setter answer) "string" (list (get-env var)) "5em"))

(tm-menu (focus-customizable-icons-item setter var name)
  (:require (parameter-choice-list var))
  (mini #t
    (=> (eval (get-env var))
        (for (val (parameter-choice-list var))
          ((eval val) (setter val))))))

(tm-menu (focus-customizable-icons-item setter var name)
  (:require (== (tree-label-type (string->symbol var)) "color"))
  (=> (color (tree->stree (get-env-tree var)) #f #f 24 16)
      (pick-background "" (setter answer))
      ---
      ("Palette" (interactive-color setter '()))
      ("Pattern" (open-pattern-selector setter "1cm"))
      ("Picture" (open-background-picture-selector setter))
      ("Other" (interactive setter (list name "string" (get-env var))))))

(tm-menu (focus-extra-icons t)
  (:require (customizable-context? t))
  (for (p (customizable-parameters t))
    (with (var name) p
      (with l (tree-label t)
        (with setter (lambda (val)
                       (when (tree-is? (focus-tree) l)
                         (tree-with-set (focus-tree) var val)))
          (glue #f #f 3 0)
          (mini #t (group (eval (string-append name ":"))))
          (dynamic (focus-customizable-icons-item setter var name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediately load document-menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (generic document-menu))
