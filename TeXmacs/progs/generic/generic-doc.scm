
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-doc.scm
;; DESCRIPTION : documentation of general tags
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-doc)
  (:use (generic generic-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Brief description of the tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (avoid-conflict name prev-names i)
  (with s (if (== i 1) name (string-append name (number->string i)))
    (if (nin? s prev-names) s
	(avoid-conflict name prev-names (+ i 1)))))

(define (focus-doc-arg-name t i prev-names)
  (with s (tree-child-name t i)
    (avoid-conflict
      (cond ((!= s "") s)
	    ((== (tree-child-type t i) "regular") "body")
	    (else (tree-child-type t i)))
      prev-names 1)))

(tm-define (focus-doc-arg-names t i prev-names)
  (if (>= i (tree-arity t)) '()
      (with s (focus-doc-arg-name t i prev-names)
	(cons s (focus-doc-arg-names t (+ i 1) (cons s prev-names))))))

(tm-generate (focus-doc-usage-args t)
  ($with l (focus-doc-arg-names t 0 '())
    ($description-aligned
      ($for (i (.. 0 (tree-arity t)))
	($with s (list-ref l i)
	  ($describe-item ($src-arg s)
	    ($if (== s "body")
		 "The main body of the macro."
		 ($begin
		   "An argument of type \x10"
                   (tree-child-type t i) "\x11."))))))))

(tm-generate (focus-doc-usage t)
  ($let* ((lab (tree-label t))
	  (l (focus-doc-arg-names t 0 '())))
    ($explain `(explain-macro ,(symbol->string lab) ,@l)
      (focus-doc-usage-args t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show current definition of the tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-source t)
  ($with s (symbol->string (tree-label t))
    ($with def (get-env-tree s)
      ($tm-fragment `(inactive* (assign ,s ,def)))
      $lf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document structured variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree->markup t)
  ($markup (symbol->string (tree-label t))))

(tm-generate (focus-doc-variants t)
  ($let* ((lab (tree-label t))
          (l (focus-variants-of t)))
    ($para
      "The " ($markup lab) " tag admits various "
      ($tmdoc-link "main/editing/man-structured-variants"
        "structured variants")
      ": " ($markup (car l))
      ($for (x (cdr l)) ", " ($markup x)) ". "
      "You may circle among these variants using the keyboard shortcuts "
      ($shortcut (variant-circulate (focus-tree) #t)) " and "
      ($shortcut (variant-circulate (focus-tree) #f)) ". "
      "You may also pick a specific variant from the "
      ($menu "Focus" (focus-tag-name lab)) " menu or the "
      ($menu (focus-tag-name lab)) " menu on the focus toolbar.")))

(tm-generate (focus-doc-numbered t)
  ($let* ((lab (tree-label t))
          (lab* (symbol-toggle-number lab)))
    ($para
      ($when (numbered-numbered? t)
        "For most style files, the " ($markup lab)
        " environment is numbered. "
        "The environment admits an unnumbered variant " ($markup lab*) ". ")
      ($when (not (numbered-numbered? t))
        "For most style files, the " ($markup lab)
        " environment is unnumbered. "
        "The environment admits a numbered variant " ($markup lab*) ". ")
      "You may toggle the numbering using the keyboard shortcut "
      ($shortcut (numbered-toggle (focus-tree))) ", the menu entry "
      ($menu "Focus" "Numbered") ", or by pressing the "
      ($tmdoc-icon "tm_numbered.xpm") " icon on the focus toolbar.")))

(tm-generate (focus-doc-alternate t)
  ($let* ((lab (tree-label t))
          (lab* (symbol-toggle-alternate lab)))
    ($para
      ($when (alternate-first? t)
        "The " ($markup lab) " environment is \x10folded\x11 "
        "and admits an unfolded variant  " ($markup lab*) ". "
        "You may unfold the environment using the keyboard shortcut "
        ($shortcut (alternate-toggle (focus-tree))) ", the menu entry "
        ($menu "Focus" "Folded") ", or by pressing the "
        ($tmdoc-icon "tm_alternate_first.xpm")
        " icon on the focus toolbar. ")
      ($when (alternate-second? t)
        "The " ($markup lab) " environment is \x10unfolded\x11 "
        "and admits a folded variant " ($markup lab*) ". "
        "You may fold the environment using the keyboard shortcut "
        ($shortcut (alternate-toggle (focus-tree))) ", the menu entry "
        ($menu "Focus" (alternate-second-name t)) ", or by pressing the "
        ($tmdoc-icon (alternate-second-icon t))
        " icon on the focus toolbar. ")
      "Folding and unfolding is performed automatically during "
      "the traversal of a document in "
      ($tmdoc-link "main/beamer/man-beamer"
        "presentation mode")
      ".")))

(tm-generate (focus-doc-toggles t)
  ($when (numbered-context? t)
    (focus-doc-numbered t))
  ($when (alternate-context? t)
    (focus-doc-alternate t)))

(tm-generate (focus-doc-toggles-variants t)
  ($when (focus-has-variants? t)
    (focus-doc-variants t))
  ($when (focus-has-toggles? t)
    (focus-doc-toggles t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stylistic preferences for tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-style-option opt)
  ($with opt-doc (tmdoc-search-style opt)
    ($when opt-doc opt-doc)
    ($when (not opt-doc)
      ($explain ($inline `(tmpackage ,opt)
                         `(explain-synopsis ,(style-get-menu-name opt)))
        ($with brief-doc (style-get-documentation opt)
          ($when brief-doc brief-doc ".")
          ($when (not brief-doc) "Undocumented style package."))))))

(tm-generate (focus-doc-parameter par)
  ($with par-doc (tmdoc-search-parameter par)
    ($when par-doc par-doc)
    ($when (not par-doc)
      ($explain `(src-var ,par)
        "A parameter of type " (tree-label-type (string->symbol par)) "."))))

(tm-generate (focus-doc-preferences t)
  ($let* ((lab (tree-label t))
          (opts (search-tag-options t))
          (pars (list-filter (search-tag-parameters t)
                             parameter-show-in-menu?))
          (ths (search-tag-themes t)))
    ($block
      ($para
        "The rendering of the " ($markup lab)
        " tag can be customized by editing the macro which defines it. "
        "This can be done by clicking on " ($menu "Edit macro")
        " button in the " ($menu "Focus" "Preferences") " menu "
        "(or in the equivalent " ($tmdoc-icon "tm_focus_prefs.xpm")
        " icon menu on the focus toolbar). "
        "You may also directly edit the macro in the style file or package "
        "where it was defined, using " ($menu "Edit source") ".")
    
      ($when (nnull? (append opts pars ths))
        ($para
          "Still using the " ($menu "Focus" "Preferences") " menu, "
          "you may also specify "
          ($when (and (nnull? opts) (null? pars))
            "style options")
          ($when (and (null? opts) (nnull? pars))
            "style parameters")
          ($when (and (nnull? opts) (nnull? pars))
            "style options and parameters")
          " that apply to the " ($markup lab) " tag. "
          "These settings are global, so they will apply to all other "
          ($markup lab) " tags in your document, and generally also to "
          "other similar tags."))

      ($when (nnull? ths)
        ($para
          "The " ($markup lab) " tag uses themes for its rendering. "
          "These themes come with their own style parameters that "
          "can be customized via "
          ($menu "Focus" "Preferences" "Theme parameters") "."))
      
      ($when (nnull? opts)
        ($folded ($strong "Style options")
          ($for (opt opts)
            (focus-doc-style-option opt))))

      ($when (nnull? pars)
        ($folded ($strong "Style parameters")
          ($for (par pars)
            (focus-doc-parameter par))))
      
      ($for (th ths)
        ($folded ($strong "Parameters for the " th " theme")
          ($for (mem (theme->members th))
            (focus-doc-parameter mem)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document structured editing operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-similar t)
  ($with lab (tree-label t)
    ($if (focus-has-variants? t)
         ($begin "tag similar to " ($markup lab))
         ($begin ($markup lab) " tag"))))

(tm-generate (focus-doc-move t)
  ($with lab (tree-label t)
    ($para
      "It is sometimes useful to quickly go through all "
      ($markup lab) " tags"
      ($when (focus-has-variants? t) " and its variants, ")
      " inside a document. "
      "This can be done efficiently using the following keyboard shortcuts, "
      "menu entries, or icons on the focus toolbar: ")
    ($description-long
      ($describe-item
          ($inline ($shortcut (kbd-select-if-active traverse-first)) ", "
                   ($menu "Focus" "First similar") ", "
                   ($tmdoc-icon "tm_similar_first.xpm"))
        "Jump to the first " (focus-doc-similar t) ".")
      ($describe-item
          ($inline ($shortcut (kbd-select-if-active traverse-previous)) ", "
                   ($menu "Focus" "Previous similar") ", "
                   ($tmdoc-icon "tm_similar_previous.xpm"))
        "Jump to the previous " (focus-doc-similar t) ".")
      ($describe-item
          ($inline ($shortcut (kbd-select-if-active traverse-next)) ", "
                   ($menu "Focus" "Next similar") ", "
                   ($tmdoc-icon "tm_similar_next.xpm"))
        "Jump to the next " (focus-doc-similar t) ".")
      ($describe-item
          ($inline ($shortcut (kbd-select-if-active traverse-last)) ", "
                   ($menu "Focus" "Last similar") ", "
                   ($tmdoc-icon "tm_similar_last.xpm"))
        "Jump to the last " (focus-doc-similar t) "."))
    ($para
      "For more information and further useful shortcuts, "
      "we refer to the section on "
      ($tmdoc-link "main/editing/man-structured-move"
        "structured cursor movements")
      ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert and remove children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-insert t)
  ($with lab (tree-label t)
    ($para
      "The " ($markup lab) " tag has a variable number of arguments. "
      "New arguments can be inserted using the following keyboard shortcuts, "
      "menu entries, or icons on the focus toolbar: ")
    ($description-long
      ($when (structured-horizontal? t)
        ($describe-item
            ($inline ($shortcut (structured-insert-left)) ", "
                     ($menu "Focus" "Insert left") ", "
                     ($tmdoc-icon "tm_insert_left.xpm"))
          "Insert a new argument on the left-hand side of the cursor.")
        ($describe-item
            ($inline ($shortcut (structured-insert-right)) ", "
                     ($menu "Focus" "Insert right") ", "
                     ($tmdoc-icon "tm_insert_right.xpm"))
          "Insert a new argument on the right-hand side of the cursor."))
      ($when (structured-vertical? t)
        ($describe-item
            ($inline ($shortcut (structured-insert-up)) ", "
                     ($menu "Focus" "Insert above") ", "
                     ($tmdoc-icon "tm_insert_up.xpm"))
          "Insert a new argument above the cursor.")
        ($describe-item
            ($inline ($shortcut (structured-insert-down)) ", "
                     ($menu "Focus" "Insert down") ", "
                     ($tmdoc-icon "tm_insert_down.xpm"))
          "Insert a new argument below the cursor.")))
    ($para
      "Existing arguments can be removed as follows:")
    ($description-long
      ($when (structured-horizontal? t)
        ($describe-item
            ($inline ($shortcut (structured-remove-left)) ", "
                     ($menu "Focus" "Remove left") ", "
                     ($tmdoc-icon "tm_delete_left.xpm"))
          "Remove the argument on the left-hand side of the cursor.")
        ($describe-item
            ($inline ($shortcut (structured-remove-right)) ", "
                     ($menu "Focus" "Remove right") ", "
                     ($tmdoc-icon "tm_delete_right.xpm"))
          "Remove the current argument and move to the next one."))
      ($when (structured-vertical? t)
        ($describe-item
            ($inline ($menu "Focus" "Remove above") ", "
                     ($tmdoc-icon "tm_delete_up.xpm"))
          "Remove the argument above the cursor.")
        ($describe-item
            ($inline ($menu "Focus" "Remove below") ", "
                     ($tmdoc-icon "tm_delete_down.xpm"))
          "Remove the current argument and move to the one below.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hidden arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-hidden t)
  ($let* ((lab (tree-label t))
	  (l (focus-doc-arg-names t 0 '()))
	  (il (list-filter (.. 0 (length l))
			   (lambda (i) (not (tree-accessible-child? t i)))))
	  (sl (map (lambda (i) (list-ref l i)) il)))
    ($para
      "When the " ($markup lab) " tag is "
      ($tmdoc-link "main/text/keyboard/man-dynamic" "activated") ", "
      "then the following arguments are hidden: "
      ($src-arg (car sl))
      ($for (x (cdr sl)) ", " ($src-arg x)) ". "
      "In order to edit the hidden arguments, you should use "
      ($menu "Focus" "Show hidden") " or push the "
      ($tmdoc-icon "tm_show_hidden.xpm") " icon on the focus toolbar. "
      "Deactivated tags can be reactivated by pressing "
      ($shortcut (kbd-return)) ".")
    ($para
      "Non internal hidden arguments which contain string values "
      "can also be edited directly in the text fields on the focus toolbar; "
      "no need to deactivate the " ($markup lab) " tag in this case.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change the geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-geometry t)
  "Not yet documented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default tag documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-doc t)
  ($tmdoc
    ($tmdoc-title "Contextual help on the \x10"
                  (symbol->string (tree-label t)) "\x11 tag")
    ($when #t
      ($unfolded-documentation "Usage"
	(focus-doc-usage t)))
    ($with tagdoc (tmdoc-search-tag (tree-label t))
      ($when tagdoc
	($unfolded-documentation "Description"
	  tagdoc)))
    ($when (tree-label-extension? (tree-label t))
      ($unfolded-documentation "Current definition"
        (focus-doc-source t)))
    ($when (or (focus-has-variants? t) (focus-has-toggles? t))
      ($unfolded-documentation "Structured variants"
        (focus-doc-toggles-variants t)))
    ($when (focus-has-preferences? t)
      ($unfolded-documentation "Style preferences"
        (focus-doc-preferences t)))
    ($when (focus-can-move? t)
      ($unfolded-documentation "Structured navigation"
        (focus-doc-move t)))
    ($when (focus-can-insert-remove? t)
      ($unfolded-documentation "Structured insert and delete"
        (focus-doc-insert t)))
    ($when (< (length (tree-accessible-children t))
              (length (tree-children t)))
      ($unfolded-documentation "Hidden arguments"
        (focus-doc-hidden t)))
    ($when (focus-has-geometry? t)
      ($unfolded-documentation "Structured geometry"
        (focus-doc-geometry t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy loading of further documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-initialize (table table-doc) (table-markup-context? (focus-tree)))
(lazy-initialize (generic document-doc) (tree-is-buffer? (focus-tree)))
(lazy-initialize (dynamic session-doc) (in-session?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-help)
  (lazy-initialize-force)
  (cursor-history-add (cursor-path))
  (open-auxiliary "Contextual help" (focus-doc (focus-tree))))
