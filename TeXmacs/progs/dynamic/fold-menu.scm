
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-menu.scm
;; DESCRIPTION : menus for folding
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-menu)
  (:use (dynamic fold-edit)
        (generic generic-menu)
	(generic document-menu)
        (generic format-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for direct folding and switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind fold-menu
  (when (with t (tree-innermost dynamic-context?)
          (and t (toggle-second-context? t)))
    ("Fold" (dynamic-previous)))
  (when (with t (tree-innermost dynamic-context?)
          (and t (toggle-first-context? t)))
    ("Unfold" (dynamic-next))))

(menu-bind switch-menu
  (when (with t (tree-innermost dynamic-context?)
          (and t (switch-context? t)))
    ("Add branch before" (switch-insert-at (focus-tree) :current #f))
    ("Add branch after" (switch-insert-at (focus-tree) :var-next #f))
    ("Remove this branch" (switch-remove-at (focus-tree) :current))
    ---
    (when (switch-index (focus-tree))
      (when (< 0 (switch-index (focus-tree)))
        ("Switch to first" (dynamic-first)))
      (when (< 0 (switch-index (focus-tree)))
        ("Switch to previous" (dynamic-previous)))
      (when (< (switch-index (focus-tree)) (switch-index (focus-tree) :last))
        ("Switch to next" (dynamic-next)))
      (when (< (switch-index (focus-tree)) (switch-index (focus-tree) :last))
        ("Switch to last" (dynamic-last))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting foldable and switchable tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-extern-converters)
  (let* ((l1 (converters-from "texmacs-snippet"))
         (l2 (filter
               (lambda (x) (and (string? x)
                                (string-ends? x "-snippet")
                                (not (string-contains? x "texmacs")))) l1))
         (l3 (map (cut string-replace <> "-snippet" "") l2))
         (l4 (sort l3 (lambda (x y) (string<? (format-get-name x)
                                              (format-get-name y))))))
    l4))

(tm-menu (supported-convertible-menu)
  (for (name (get-extern-converters))
       ((eval (format-get-name name))
        (insert-go-to `(converter-input ,name "" "") '(1 0)))))

(tm-menu (supported-executable-menu)
  (for (name (session-list))
    (with menu-name (session-name name)
      ((eval menu-name)
       (insert-go-to `(script-input ,name "default" "" "") '(2 0))))))

(menu-bind insert-fold-menu
  (-> "Folded"
      ("Default" (make-toggle 'folded))
      ---
      ("Plain" (make-toggle 'folded-plain))
      ("Standard" (make-toggle 'folded-std))
      ("Environment" (make-toggle 'folded-env))
      ("Documentation" (make-toggle 'folded-documentation))
      ("Grouped" (make-toggle 'folded-grouped)))
  (-> "Summarized"
      ("Default" (make-toggle 'summarized))
      ---
      ("Plain" (make-toggle 'summarized-plain))
      ("Standard" (make-toggle 'summarized-std))
      ("Environment" (make-toggle 'summarized-env))
      ("Documentation" (make-toggle 'summarized-documentation))
      ("Grouped" (make-toggle 'summarized-grouped)))
  (-> "Switch"
      ("Standard" (make-switch 'switch))
      (when (not (screens-buffer?))
        ("Screens" (make-screens)))
      ("Tiny" (make-switch 'tiny-switch)))
  (-> "Unroll"
      ("Default" (make-unroll 'unroll))
      ("Compressed" (make-unroll 'unroll-compressed))
      ("Phantoms" (make-unroll 'unroll-phantoms))
      ("Greyed" (make-unroll 'unroll-greyed))
      ---
      ("Itemize" (make-switch-list 'unroll 'itemize))
      ("Enumerate" (make-switch-list 'unroll 'enumerate))
      ("Description" (make-switch-list 'unroll 'description)))
  (-> "Expanded"
      ("Standard" (make-switch 'expanded))
      ("Slides" (make-switch 'slides)))
  (if (not (tree-innermost overlays-context?))
      (-> "Overlays"
          ("Standard" (make-overlays 'overlays))
          ("Compressed" (make-overlays 'overlays-compressed))
          ("Phantoms" (make-overlays 'overlays-phantoms))
          ("Greyed" (make-overlays 'overlays-greyed))))
  (if (tree-innermost overlays-context?)
      (-> "Overlay"
          ("Specify color alternation" (make-alter-colors))
          ---
          ("Visible from here on" (make-overlay 'overlay-from))
          ("Visible until here" (make-overlay 'overlay-until))
          ("Visible only here" (make-overlay 'overlay-this))
          ("Visible except here" (make-overlay 'overlay-other))
          ---
          ("Alternate from here on" (make-overlay 'alternate-from))
          ("Alternate until here" (make-overlay 'alternate-until))
          ("Alternate only here" (make-overlay 'alternate-this))
          ("Alternate except here" (make-overlay 'alternate-other))))
  (-> "Convertible" (link supported-convertible-menu))
  (if (!= (session-list) '())
      (-> "Executable"
          (link supported-executable-menu)))
  ;;(-> "Hidden content"
  ;;    ("Deleted" (make 'hidden-deleted))
  ;;    ("Invisible" (make 'hidden-invisible))
  ;;    ("Greyed" (make 'hidden-greyed)))
  (-> "Traversal"
      ("Fold back" (make 'fold-back))
      ("Keep unfolded" (make 'keep-unfolded))
      (if #f
          ("Animate folding" (noop))
          ("Animate unfolding" (noop))))
  (if (== (get-preference "experimental encryption") "on")
      (-> "Encrypt" (link gpg-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operate on buffers with dynamic markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-menu (fold/unfold-menu-entry x which action)
  (with sym (string->symbol x)
    (when (ahash-ref which sym)
      ((eval (upcase-first x))
       (dynamic-operate-on-buffer (list action sym))))))

(tm-menu (fold-environments-menu)
  (receive (l first second) (fold-get-environments-in-buffer)
    (assuming (nnull? l) ---)
    (for (x l) (dynamic (fold/unfold-menu-entry x second :fold)))))

(tm-menu (unfold-environments-menu)
  (receive (l first second) (fold-get-environments-in-buffer)
    (assuming (nnull? l) ---)
    (for (x l) (dynamic (fold/unfold-menu-entry x second :unfold)))))

(menu-bind dynamic-menu
  ("First" (dynamic-operate-on-buffer :first))
  (if (in-screens?)
      ("Previous screen" (screens-switch-to :previous)))
  ("Previous" (dynamic-traverse-buffer :previous))
  ("Next" (dynamic-traverse-buffer :next))
  (if (in-screens?)
      ("Next screen" (screens-switch-to :next)))
  ("Last" (dynamic-operate-on-buffer :last))
  ---
  (-> "Fold"
      ("All" (dynamic-operate-on-buffer :fold))
      (link fold-environments-menu))
  (-> "Unfold"
      ("All" (dynamic-operate-on-buffer :unfold))
      (link unfold-environments-menu))
  (-> "Compress"
      ("Preserve tags" (dynamic-operate-on-buffer :compress))
      ("Change tags" (dynamic-operate-on-buffer :var-compress)))
  (-> "Expand"
      ("Preserve tags" (dynamic-operate-on-buffer :expand))
      ("Change tags" (dynamic-operate-on-buffer :var-expand)))
  (-> "Filter"
      ("Remove folded" (dynamic-filter-buffer :remove-folded))
      ("Remove unfolded" (dynamic-filter-buffer :remove-unfolded))
      ("Only keep folded" (dynamic-filter-buffer :keep-folded))
      ("Only keep unfolded" (dynamic-filter-buffer :keep-unfolded))))

(tm-define (alternate-second-name t)
  (:require (fold-context? t))
  "Unfold")

(tm-define (alternate-second-icon t)
  (:require (fold-context? t))
  "tm_alternate_both.xpm")

(menu-bind dynamic-icons
  ((balloon (icon "tm_larrow_bar.xpm") "First")
   (dynamic-operate-on-buffer :first))
  (if (in-screens?)
      ((balloon (icon "tm_larrow_double.xpm") "Previous screen")
       (screens-switch-to :previous)))
  ((balloon (icon "tm_larrow.xpm") "Previous")
   (dynamic-traverse-buffer :previous))
  ((balloon (icon "tm_rarrow.xpm") "Next")
   (dynamic-traverse-buffer :next))
  (if (in-screens?)
      ((balloon (icon "tm_rarrow_double.xpm") "Next screen")
       (screens-switch-to :next)))
  ((balloon (icon "tm_rarrow_bar.xpm") "Last")
   (dynamic-operate-on-buffer :last)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beamer themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-style-extra-menu
  (:require (style-has? "beamer-style"))
  (=> "Beamer theme"
      (for (theme (beamer-themes))
        ((check (eval (upcase-first theme)) "v" (has-style-package? theme))
         (add-style-package theme)))))

(tm-menu (focus-style-extra-menu t)
  (:require (style-has? "beamer-style"))
  (=> "Beamer theme"
      (for (theme (beamer-themes))
        ((check (eval (upcase-first theme)) "v" (has-style-package? theme))
         (add-style-package theme)))))

(tm-menu (focus-style-extra-icons t)
  (:require (style-has? "beamer-style"))
  (=> (balloon (eval (upcase-first (current-beamer-theme))) "Beamer theme")
      (for (theme (beamer-themes))
        ((check (eval (upcase-first theme)) "v" (has-style-package? theme))
         (add-style-package theme)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propose insertion of 'screens' tag in beamer style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-screens)
  (let* ((t (buffer-tree))
         (l (tree-children t))
         (p (cursor-inside? t)))
    (if (and (tree-is? t 'document)
             (tree-in? t 0 '(hide-preamble show-preamble)))
        (begin
          (tree-assign! t `(document ,(tree-ref t 0)
                                     (screens (shown (document ,@(cdr l))))))
          (if (and p (!= (car p) 0))
              (apply tree-go-to `(,t 1 0 0 ,(- (car p) 1) ,@(cdr p)))))
        (begin
          (tree-assign! t `(document (screens (shown (document ,@l)))))
          (if p (apply tree-go-to `(,t 0 0 0 ,@p)))))))

(tm-define (remove-single-screens)
  (let* ((t (buffer-tree))
         (l (tree-children t)))
    (when (tree-func? (cAr l) 'screens 1)
      (when (tree-func? (tree-ref (cAr l) 0) 'shown 1)
        (let* ((d (tree-ref (tree-ref (cAr l) 0) 0))
               (b (tree->path t))
               (p (cursor-inside? d)))
          (when (tree-func? d 'document)
            (with q (and (pair? p)
                         (cons (+ (car p) (length (cDr l))) (cdr p)))
              (tree-assign! t `(document ,@(cDr l) ,@(tree-children d)))
              (if q (delayed (:idle 1) (go-to-path (append b q)))))))))))

(tm-define (document-propose-screens?)
  (and (style-has? "beamer-style")
       (not (screens-buffer?))))

(tm-menu (focus-document-extra-menu t)
  (:require (document-propose-screens?))
  ("Screens" (make-screens)))

(tm-menu (focus-document-extra-icons t)
  (:require (document-propose-screens?))
  (minibar
    ((balloon "Screens" "Make a multi-slide presentation")
     (make-screens))))

(tm-define (notify-new-style style)
  (former style)
  (if (style-has? "beamer-style")
      (if (not (screens-buffer?)) (make-screens))
      (if (screens-buffer?) (remove-single-screens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slide titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-options l)
  (:require (== l 'tit))
  (list "framed-title" "title-bar"))

(tm-define (parameter-show-in-menu? l)
  (:require (== l "title-theme"))
  #f)

(tm-define (slide-propose-title? t)
  (and-with u (slide-get-document t)
    (not (tree-is? u 0 'tit))))

(tm-define (slide-insert-title t)
  (and-with u (slide-get-document t)
    (tree-insert u 0 '((tit "")))
    (tree-go-to u 0 0 0)))

(tm-define (search-slide-name t*)
  (with t (slide-get-document t*)
    (if (and (tree-is? t 'document)
             (tree-is? t 0 'tit))
        (texmacs->code (verbatim-expand (tm-ref t 0 0)) "cork")
        "")))

(tm-define (get-slide-name t i)
  (with s (search-slide-name t)
    (string-append "Slide " (number->string (+ i 1))
                   (if (== s "") "" (string-append ": " s)))))

(tm-menu (focus-slides-menu t*)
  (with t (slide-get-switch t*)
    (for (i (.. 0 (tree-arity t)))
      ((eval (get-slide-name (tree-ref t i) i))
       (screens-switch-to i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphical slides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (slide-propose-graphics? t)
  (and-with u (slide-get-document t)
    (or (tm-equal? u '(document ""))
        (and (tree-func? u 'document 1)
             (tree-is? u 0 'tit))
        (and (tree-func? u 'document 2)
             (tree-is? u 0 'tit)
             (tm-equal? (tree-ref u 1) "")))))

(tm-define (slide-insert-graphics t)
  (and-with u (slide-get-document t)
    (when (and (tree-func? u 'document 1)
               (tree-is? u 0 'tit))
      (tree-insert! u 1 (list "")))
    (tree-set u :last `(gr-screen (document "")))
    (tree-go-to u :last 0 0 0)
    (make-graphics
     "gr-mode" "point"
     "gr-frame" `(tuple "scale" "1cm" (tuple "0gw" "1gh"))
     "gr-geometry" `(tuple "geometry" "1gpar" "1gpag" "axis"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slide background color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (slide-page-formatter quit)
  (let* ((col (tm->stree (slide-get-bg-color)))
         (setter (lambda (c)
                   (set! col c)
                   (slide-set-bg-color col)
                   (refresh-now "slide-color-sample"))))
    (padded
      (bold (text "Background color"))
      ===
      (hlist
        (refreshable "slide-color-sample"
          (resize "150px" "100px"
            (texmacs-output `(document
                               (block
                                (tformat
                                 (cwith "1" "1" "1" "1" "cell-width" "140px")
                                 (cwith "1" "1" "1" "1" "cell-height" "90px")
                                 (cwith "1" "1" "1" "1" "cell-vmode" "exact")
                                 (cwith "1" "1" "1" "1" "cell-background" ,col)
                                 (table (row (cell ""))))))
                            `(style (tuple "generic")))))
        // // //
        (explicit-buttons
          (vlist
            ("Color" (interactive-color setter (list)))
            ("Pattern" (open-pattern-selector setter "1cm"))
            ("Picture" (open-picture-selector setter))
            (glue #f #t 0 0))))
      ======
      (explicit-buttons
        (hlist
          >>>
          ("Ok" (quit)))))))

(tm-define (open-page-format)
  (:require (or (inside? 'screens) (inside? 'slideshow)))
  (dialogue-window slide-page-formatter noop "Page format"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus when focus is on 'screens' tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-move? t)
  (:require (screens-context? t))
  #f)

(tm-menu (focus-tag-menu t)
  (:require (screens-context? t))
  (inert ((eval (focus-tag-name (tree-label t))) (noop) (noop)))
  (-> (eval (upcase-first (get-init "page-type")))
      (link document-page-size-menu))
  (-> (eval (upcase-first (get-init-page-rendering)))
      (link page-rendering-menu))
  (-> "Preferences"
      (dynamic (focus-preferences-menu t)))
  ("Describe" (focus-help)))

(tm-menu (standard-focus-menu t)
  (:require (screens-context? t))
  (dynamic (focus-style-menu t))
  ---
  (dynamic (focus-tag-menu t))
  ---
  (dynamic (focus-insert-menu t))
  ---
  (dynamic (focus-slides-menu t))
  (assuming (slide-propose-title? t)
    ---
    ("Title" (slide-insert-title t)))
  (assuming (slide-propose-graphics? t)
    ---
    ("Draw" (slide-insert-graphics t))))

(tm-menu (focus-tag-icons t)
  (:require (screens-context? t))
  (mini #t (inert ((eval (focus-tag-name (tree-label t))) (noop))))
  (=> (balloon (eval (upcase-first (get-init "page-type")))
               "Paper size")
      (link document-page-size-menu))
  (=> (balloon (eval (upcase-first (font-family-main (get-init "font"))))
               "Main document font")
      (link document-short-font-menu))
  (=> (balloon (eval (string-append (get-init "font-base-size") "pt"))
               "Font size")
      (link document-font-base-size-menu))
  (=> (balloon (icon (eval (current-page-icon))) "Page layout")
      (link page-rendering-menu))
  (assuming (focus-has-preferences? t)
    (=> (balloon (icon "tm_focus_prefs.xpm") "Preferences for tag")
	(dynamic (focus-preferences-menu t))))
  ((balloon (icon "tm_focus_help.xpm") "Describe tag")
   (focus-help)))

(tm-menu (standard-focus-icons t)
  (:require (screens-context? t))
  (dynamic (focus-style-icons t))  
  //
  (minibar (dynamic (focus-insert-icons t)))
  //
  (minibar (dynamic (focus-tag-icons t)))
  //
  (with u (slide-get-switch t)
    (with i (tree-index (tree-down u))
      (mini #t
        (=> (eval (get-slide-name (tree-ref u i) i))
            (dynamic (focus-slides-menu t))))))
  (assuming (slide-propose-title? t)
    //
    (minibar
     ((balloon "Title" "Insert title") (slide-insert-title t))))
  (assuming (slide-propose-graphics? t)
    //
    (minibar
     ((balloon "Draw" "Draw graphics") (slide-insert-graphics t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu customizations for overlays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-insert-remove? t)
  (:require (overlays-context? t))
  #t)

(tm-define (focus-can-insert? t)
  (:require (overlays-context? t))
  #t)

(tm-define (focus-can-remove? t)
  (:require (overlays-context? t))
  #t)

(tm-define (parameter-show-in-menu? l)
  (:require (== l "overlay-nr"))
  #f)

(define (get-overlays-menu-name prefix t)
  (let* ((cur (overlays-current t))
         (tot (overlays-arity t)))
    (set! cur (if cur (number->string cur) "?"))
    (set! tot (if tot (number->string tot) "?"))
    (string-append prefix cur "/" tot)))

(tm-menu (focus-overlays-menu t)
  (for (i (.. 1 (or (+ (overlays-arity t) 1) 2)))
    ((eval (string-append "Overlay " (number->string i)))
     (overlays-switch-to t i))))

(tm-menu (focus-hidden-menu t)
  (:require (overlays-context? t))
  ---
  (dynamic (focus-overlays-menu t)))

(tm-menu (focus-hidden-icons t)
  (:require (overlays-context? t))
  //
  (=> (eval (get-overlays-menu-name "Overlay " t))
      (dynamic (focus-overlays-menu t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu customizations for overlay filters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-overlay-icon t i here?)
  ((eval (if here?
             (string-append "[" (number->string i) "]")
             (number->string i)))
   (tree-set t 0 (number->string i))))

(tm-menu (focus-overlay-icons t)
  (for (i (.. 1 (or (+ (overlay-arity t) 1) 2)))
    (if (overlay-visible? t i)
        (bold (dynamic (focus-overlay-icon t i (== i (overlay-current t))))))
    (if (not (overlay-visible? t i))
        (grey (dynamic (focus-overlay-icon t i (== i (overlay-current t))))))))

(tm-menu (focus-hidden-icons t)
  (:require (overlay-context? t))
  //
  (dynamic (focus-overlay-icons t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlays in graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (proviso-name p)
  (cond ((== p "mixed") "mixed")
        ((tm-func? p 'show-this 1) "=")
        ((tm-func? p 'show-from 1) ">=")
        ((tm-func? p 'show-until 1) "<=")
        (else "*")))

(tm-menu (graphics-overlays-mode-menu)
  (let* ((t (tree-innermost overlays-context?))
         (cur (and t (number->string (overlays-current t)))))
    ("This" (graphics-set-proviso `(show-this ,cur)))
    ("From" (graphics-set-proviso `(show-from ,cur)))
    ("Until" (graphics-set-proviso `(show-until ,cur)))
    ("Always" (graphics-set-proviso "default"))))

(tm-menu (graphics-focus-overlays-menu)
  (with t (tree-innermost overlays-context?)
    (assuming (nnot t)
      ---
      (-> "Overlay mode"
          (link graphics-overlays-mode-menu))
      (-> (eval (get-overlays-menu-name "Overlay " t))
          (dynamic (focus-overlays-menu t))))))

(tm-menu (graphics-focus-overlays-icons)
  (with t (tree-innermost overlays-context?)
    (assuming (nnot t)
      /
      (mini #t
        (group "Overlay:")
        (=> (eval (proviso-name (graphics-get-proviso)))
            (link graphics-overlays-mode-menu))
        (=> (eval (get-overlays-menu-name "" t))
            (dynamic (focus-overlays-menu t)))))))

(tm-menu (graphics-screens-menu)
  (with t (tree-innermost screens-context?)
    (assuming (nnot t)
      ("Insert slide before" (structured-insert-horizontal t #f))
      ("Insert slide after"  (structured-insert-horizontal t #t))
      ("Remove slide before" (structured-remove-horizontal t #f))
      ("Remove slide after"  (structured-remove-horizontal t #t)))))

(tm-menu (graphics-overlays-manage-menu)
  (with t (tree-innermost overlays-context?)
    (assuming (nnot t)
      ("Insert overlay before" (structured-insert-horizontal t #f))
      ("Insert overlay after"  (structured-insert-horizontal t #t))
      ("Remove overlay before" (structured-remove-horizontal t #f))
      ("Remove overlay after"  (structured-remove-horizontal t #t)))))

(tm-menu (graphics-overlays-menu)
  (with t (tree-innermost overlays-context?)
    (assuming (not t)
      ("Insert overlay before" (make-gr-overlays #f))
      ("Insert overlay after" (make-gr-overlays #t)))
    (assuming t
      (link graphics-overlays-manage-menu))))
