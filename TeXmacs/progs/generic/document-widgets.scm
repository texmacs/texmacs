
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-widgets.scm
;; DESCRIPTION : widgets for setting global document properties
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-widgets)
  (:use (generic document-menu)
        (generic format-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style chooser widget (still to be implemented)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (select-style-among-widget l)
  (resize ("300px" "300px" "300px") ("200px" "300px" "1000px")
    (scrollable
      (choice (set-main-style answer) l "generic"))))

(tm-widget (select-common-style-widget)
  (dynamic (select-style-among-widget
            (list "article" "beamer" "book" "browser" "exam"
                  "generic" "letter" "manual" "seminar" "source"))))

(tm-widget (select-education-style-widget)
  (dynamic (select-style-among-widget
            (list "compact" "exam"))))

(tm-widget (select-article-style-widget)
  (dynamic (select-style-among-widget
            (list "article" "tmarticle"))))

(tm-widget (select-any-style-widget)
  (dynamic (select-style-among-widget
            (list "article" "tmarticle"))))

(tm-widget (select-style-widget)
  (tabs
    (tab (text "Common")
      (dynamic (select-common-style-widget)))
    (tab (text "Education")
      (dynamic (select-education-style-widget)))
    (tab (text "Article")
      (dynamic (select-article-style-widget)))
    (tab (text "Any")
      (dynamic (select-any-style-widget)))))

(tm-define (open-style-selector)
  (:interactive #t)
  (top-window select-style-widget "Select document style"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Source -> Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((source-tree-preferences-editor u) quit)
  (padded
    (refreshable "source-tree-preferences"
      (aligned
        (item (text "Main presentation style:")
          (enum (initial-set u "src-style" answer)
                '("angular" "scheme" "functional" "latex")
                (initial-get u "src-style") "10em"))
        (item (text "Tags with special rendering:")
          (enum (initial-set u "src-special" answer)
                '("raw" "format" "normal" "maximal")
                (initial-get u "src-special") "10em"))
        (item (text "Compactification:")
          (enum (initial-set u "src-compact" answer)
                '("none" "inline" "normal" "inline args" "all")
                (initial-get u "src-compact") "10em"))
        (item (text "Closing style:")
          (enum (initial-set u "src-close" answer)
                '("repeat" "long" "compact" "minimal")
                (initial-get u "src-close") "10em"))))
    ======
    (explicit-buttons
      (hlist
        >>>
        ("Reset"
         (initial-default u "src-style" "src-special"
                            "src-compact" "src-close")
         (refresh-now "source-tree-preferences"))
        // //
        ("Ok" (quit))))))

(tm-define (open-source-tree-preferences)
  (:interactive #t)
  (with u (current-buffer)
    (dialogue-window (source-tree-preferences-editor u) noop
                     "Document source tree preferences")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Paragraph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-document-paragraph-format)
  (:interactive #t)
  (let* ((old (get-init-table paragraph-parameters))
         (new (get-init-table paragraph-parameters))
         (u   (current-buffer)))
    (dialogue-window (paragraph-formatter old new init-multi u #t)
                     noop "Document paragraph format")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (page-size-list u)
  (if (initial-defined? u "beamer-style")
      (list "16:9" "8:5" "4:3" "5:4" "user")
      (list "a3" "a4" "a5" "b4" "b5" "letter" "legal" "executive" "user")))

(define (user-page-size? u)
  (== (initial-get u "page-type") "user"))

(tm-widget (page-formatter-format u quit)
  (centered
    (refreshable "page-format-settings"
      (aligned
        (item (text "Screen rendering:")
          (enum (initial-set-page-rendering u answer)
                '("paper" "papyrus" "automatic" "beamer" "book" "panorama")
                (initial-get-page-rendering u) "10em"))
        (item (text "Page type:")
          (enum (begin
                  (initial-set u "page-type" answer)
                  (when (!= answer "user")
                    (initial-set u "page-width" "auto")
                    (initial-set u "page-height" "auto"))
                  (refresh-now "page-user-format-settings"))
                (cons-new (initial-get u "page-type") (page-size-list u))
                (initial-get u "page-type") "10em"))
        (item (text "Orientation:")
          (enum (initial-set u "page-orientation" answer)
                '("portrait" "landscape")
                (initial-get u "page-orientation") "10em"))
        (item (text "First page:")
          (enum (initial-set u "page-first" answer)
                (list (initial-get u "page-first") "")
                (initial-get u "page-first") "10em")))))
  ===
  (centered
    (refreshable "page-user-format-settings"
      (when (== (initial-get u "page-type") "user")
        (aligned
          (item (when (user-page-size? u) (text "Page width:"))
            (when (user-page-size? u)
              (enum (initial-set u "page-width" answer)
                    (list (initial-get u "page-width") "")
                    (initial-get u "page-width") "10em")))
          (item (when (user-page-size? u) (text "Page height:"))
            (when (user-page-size? u)
              (enum (initial-set u "page-height" answer)
                    (list (initial-get u "page-height") "")
                    (initial-get u "page-height") "10em")))))))
  ======
  (explicit-buttons
   (hlist
     >>>
     ("Reset"
      (initial-default u "page-medium" "page-type" "page-orientation"
                         "page-border" "page-packet" "page-offset"
                         "page-width" "page-height")
      (refresh-now "page-format-settings")
      (refresh-now "page-user-format-settings"))
     // //
     ("Ok" (quit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Margins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-formatter-margins u quit)
  (padded
    (refreshable "page-margin-toggles"
      (centered
        (aligned
          (meti (hlist // (text "Determine margins from text width"))
            (toggle (begin
                      (initial-set u "page-width-margin"
                                   (if answer "true" "false"))
                      (refresh-now "page-margin-settings"))
                    (== (initial-get u "page-width-margin") "true")))
          (meti (hlist // (text "Same screen margins as on paper"))
            (toggle (begin
                      (initial-set u "page-screen-margin"
                                   (if answer "false" "true"))
                      (refresh-now "page-screen-margin-settings"))
                    (!= (initial-get u "page-screen-margin") "true"))))))
    ======
    (hlist
      (refreshable "page-margin-settings"
        (hlist (bold (text "Margins on paper")))
        === ===
        (if (!= (initial-get u "page-width-margin") "true")
            (aligned
              (item (text "(Odd page) Left:")
                (input (initial-set u "page-odd" answer) "string"
                       (list (initial-get u "page-odd")) "6em"))
              (item (text "(Even page) Left:")
                (input (initial-set u "page-even" answer) "string"
                       (list (initial-get u "page-odd")) "6em"))
              (item (text "(Odd page) Right:")
                (input (initial-set u "page-right" answer) "string"
                       (list (initial-get u "page-right")) "6em"))
              (item (text "Top:")
                (input (initial-set u "page-top" answer) "string"
                       (list (initial-get u "page-top")) "6em"))
              (item (text "Bottom:")
                (input (initial-set u "page-bot" answer) "string"
                       (list (initial-get u "page-bot")) "6em"))))
        (if (== (initial-get u "page-width-margin") "true")
            (aligned
              (item (text "Text width:")
                (input (initial-set u "par-width" answer) "string"
                       (list (initial-get u "par-width")) "6em"))
              (item (text "Odd page shift:")
                (input (initial-set u "page-odd-shift" answer) "string"
                       (list (initial-get u "page-odd-shift")) "6em"))
              (item (text "Even page shift:")
                (input (initial-set u "page-even-shift" answer) "string"
                       (list (initial-get u "page-even-shift")) "6em"))
              (item (text "Top:")
                (input (initial-set u "page-top" answer) "string"
                       (list (initial-get u "page-top")) "6em"))
              (item (text "Bottom:")
                (input (initial-set u "page-bot" answer) "string"
                       (list (initial-get u "page-bot")) "6em"))))
        (glue #f #t 0 0))
      /// ///
      (refreshable "page-screen-margin-settings"
        (when (== (initial-get u "page-screen-margin") "true")
          (hlist (bold (text "Margins on screen")))
          === ===
          (aligned
            (item (text "Left:")
              (input (initial-set u "page-screen-left" answer) "string"
                     (list (initial-get u "page-screen-left")) "6em"))
            (item (text "Right:")
              (input (initial-set u "page-screen-right" answer) "string"
                     (list (initial-get u "page-screen-right")) "6em"))
            (item (text "Top:")
              (input (initial-set u "page-screen-top" answer) "string"
                     (list (initial-get u "page-screen-top")) "6em"))
            (item (text "Bottom:")
              (input (initial-set u "page-screen-bot" answer) "string"
                     (list (initial-get u "page-screen-bot")) "6em")))
          (glue #f #t 0 0)))))
  ======
  (explicit-buttons
   (hlist
     >>>
     ("Reset"
      (initial-default u "page-odd" "page-even" "page-right"
                         "page-top" "page-bot" "par-width"
                         "page-odd-shift" "page-even-shift"
                         "page-screen-left" "page-screen-right"
                         "page-screen-top" "page-screen-bot"
                         "page-width-margin" "page-screen-margin")
      (refresh-now "page-margin-toggles")
      (refresh-now "page-margin-settings")
      (refresh-now "page-screen-margin-settings"))
     // //
     ("Ok" (quit)))))

(tm-widget (page-formatter-margins u quit)
  (:require (style-has? "std-latex-dtd"))
  (padded
    (centered
      (text "This style specifies page margins in the TeX way"))
    ===
    (refreshable "page-margin-toggles"
      (centered
        (aligned
          (meti (hlist // (text "Same screen margins as on paper"))
            (toggle (begin
                      (initial-set u "page-screen-margin"
                                   (if answer "false" "true"))
                      (refresh-now "page-screen-margin-settings"))
                    (!= (initial-get u "page-screen-margin") "true"))))))
    ======
    (hlist
      (refreshable "page-tex-hor-margins"
        (hlist (bold (text "Horizontal margins")))
        === ===
	(aligned
	  (item (text "oddsidemargin:")
	    (input (initial-set u "tex-odd-side-margin" answer) "string"
		   (list (initial-get u "tex-odd-side-margin")) "6em"))
	  (item (text "evensidemargin:")
	    (input (initial-set u "tex-even-side-margin" answer) "string"
		   (list (initial-get u "tex-even-side-margin")) "6em"))
	  (item (text "textwidth:")
	    (input (initial-set u "tex-text-width" answer) "string"
		   (list (initial-get u "tex-text-width")) "6em"))
	  (item (text "linewidth:")
	    (input (initial-set u "tex-line-width" answer) "string"
		   (list (initial-get u "tex-line-width")) "6em"))
	  (item (text "columnwidth:")
	    (input (initial-set u "tex-column-width" answer) "string"
		   (list (initial-get u "tex-column-width")) "6em")))
        (glue #f #t 0 0))
      /// ///
      (refreshable "page-tex-ver-margins"
        (hlist (bold (text "Vertical margins")))
        === ===
	(aligned
	  (item (text "topmargin:")
	    (input (initial-set u "tex-top-margin" answer) "string"
		   (list (initial-get u "tex-top-margin")) "6em"))
	  (item (text "headheight:")
	    (input (initial-set u "tex-head-height" answer) "string"
		   (list (initial-get u "tex-head-height")) "6em"))
	  (item (text "headsep:")
	    (input (initial-set u "tex-head-sep" answer) "string"
		   (list (initial-get u "tex-head-sep")) "6em"))
	  (item (text "textheight:")
	    (input (initial-set u "tex-text-height" answer) "string"
		   (list (initial-get u "tex-text-height")) "6em"))
	  (item (text "footskip:")
	    (input (initial-set u "tex-foot-skip" answer) "string"
		   (list (initial-get u "tex-foot-skip")) "6em")))
        (glue #f #t 0 0))
      /// ///
      (refreshable "page-screen-margin-settings"
        (when (== (initial-get u "page-screen-margin") "true")
          (hlist (bold (text "Margins on screen")))
          === ===
          (aligned
            (item (text "Left:")
              (input (initial-set u "page-screen-left" answer) "string"
                     (list (initial-get u "page-screen-left")) "6em"))
            (item (text "Right:")
              (input (initial-set u "page-screen-right" answer) "string"
                     (list (initial-get u "page-screen-right")) "6em"))
            (item (text "Top:")
              (input (initial-set u "page-screen-top" answer) "string"
                     (list (initial-get u "page-screen-top")) "6em"))
            (item (text "Bottom:")
              (input (initial-set u "page-screen-bot" answer) "string"
                     (list (initial-get u "page-screen-bot")) "6em")))
          (glue #f #t 0 0)))))
  ======
  (explicit-buttons
   (hlist
     >>>
     ("Reset"
      (initial-default u "tex-odd-side-margin" "tex-even-side-margin"
		         "tex-text-width" "tex-line-width" "tex-column-width"
                         "tex-top-margin" "tex-head-height" "tex-head-sep"
                         "tex-text-height" "tex-foot-skip"
                         "page-screen-left" "page-screen-right"
                         "page-screen-top" "page-screen-bot"
                         "page-width-margin" "page-screen-margin")
      (refresh-now "page-margin-toggles")
      (refresh-now "page-tex-hor-margins")
      (refresh-now "page-tex-ver-margins")
      (refresh-now "page-screen-margin-settings"))
     // //
     ("Ok" (quit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-formatter-breaking u quit)
  (padded
    (refreshable "page-breaking-settings"
      (aligned
        (item (text "Page breaking algorithm:")
          (enum (initial-set u "page-breaking" answer)
                '("sloppy" "professional")
                (initial-get u "page-breaking") "10em"))
        (item (text "Allowed page height reduction:")
          (enum (initial-set u "page-shrink" answer)
                (cons-new (initial-get u "page-shrink")
                          '("0cm" "0.5cm" "1cm" ""))
                (initial-get u "page-shrink") "10em"))
        (item (text "Allowed page height extension:")
          (enum (initial-set u "page-extend" answer)
                (cons-new (initial-get u "page-extend")
                          '("0cm" "0.5cm" "1cm" ""))
                (initial-get u "page-extend") "10em"))
        (item (text "Vertical space stretchability:")
          (enum (initial-set u "page-flexibility" answer)
                (cons-new (initial-get u "page-flexibility")
                          '("0" "0.25" "0.5" "0.75" "1" ""))
                (initial-get u "page-flexibility") "10em")))))
  === ===
  (explicit-buttons
   (hlist
     >>>
     ("Reset"
      (initial-default u "page-breaking" "page-shrink"
                         "page-extend" "page-flexibility")
      (refresh-now "page-breaking-settings"))
     // //
     ("Ok" (quit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define header-parameters
  (list "page-odd-header" "page-even-header"
        "page-odd-footer" "page-even-footer"))

(define (header-buffer var)
  (string->url (string-append "tmfs://aux/" var)))

(define (header-buffers)
  (map header-buffer header-parameters))

(define (get-field-contents u)
  (and-with t (tm->stree (buffer-get-body u))
    (when (tm-func? t 'document 1)
      (set! t (tm-ref t 0)))
    t))

(define (apply-headers-settings u)
  (with l (list)
    (for (var header-parameters)
      (and-with doc (get-field-contents (string-append "tmfs://aux/" var))
        (set! l (cons `(,var ,doc) l))))
    (when (nnull? l)
      (delayed
        (:idle 10)
        (for (x l) (initial-set-tree u (car x) (cadr x)))
        (refresh-window)))))

(define (editing-headers?)
  (in? (current-buffer)
       (map (lambda (x) (string->url (string-append "tmfs://aux/" x)))
            header-parameters)))

(tm-widget (page-formatter-headers u style quit)
  (padded
    (refreshable "page-header-settings"
      (for (var header-parameters)
        (bold (text (eval (parameter-name var))))
        ===
        (resize "600px" "60px"
          (texmacs-input `(document ,(initial-get-tree u var))
                         `(style (tuple ,@style))
                         (header-buffer var)))
        ===)))
  === ===
  (explicit-buttons
   (hlist
     (text "Insert:")
     // //
     ("Tab" (when (editing-headers?) (make-htab "5mm")))
     // //
     ("Page number" (when (editing-headers?) (make 'page-the-page)))
     >>>
     ;;("Reset"
     ;; (initial-default u header-parameters)
     ;; (refresh-now "page-header-settings"))
     ;;// //
     ("Ok" (apply-headers-settings u) (quit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((document-page-formatter u style) quit)
  (padded
    (tabs
      (tab (text "Format")
        (padded
          (dynamic (page-formatter-format u quit))))
      (tab (text "Margins")
        (padded
          (dynamic (page-formatter-margins u quit))))
      (tab (text "Breaking")
        (padded
          (dynamic (page-formatter-breaking u quit))))
      (tab (text "Headers")
        (padded
          (dynamic (page-formatter-headers u style quit)))))))

(tm-define (open-document-page-format)
  (:interactive #t)
  (let* ((u  (current-buffer))
         (st (list-remove-duplicates (rcons (get-style-list) "macro-editor"))))
    (apply dialogue-window
           (cons* (document-page-formatter u st)
                  noop "Document page format"
                  (header-buffers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((document-metadata-editor u) quit)
  (padded
    (refreshable "document-metadata"
      (aligned
        (item (text "Title:")
          (input (initial-set u "global-title" answer) "string"
                 (list (buffer-get-metadata u "title")) "30em"))
        (item (text "Author:")
          (input (initial-set u "global-author" answer) "string"
                 (list (buffer-get-metadata u "author")) "30em"))
        (item (text "Subject:")
          (input (initial-set u "global-subject" answer) "string"
                 (list (buffer-get-metadata u "subject")) "30em"))))
    ======
    (explicit-buttons
      (hlist
        >>>
        ("Reset"
         (initial-default u "global-title" "global-author" "global-subject")
         (refresh-now "document-metadata"))
        // //
        ("Ok" (quit))))))

(tm-define (open-document-metadata)
  (:interactive #t)
  (let* ((u (current-buffer)))
    (dialogue-window (document-metadata-editor u) noop "Document metadata")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-colors-background u)
  (pick-background "" (initial-set-tree u "bg-color" answer)))

(tm-widget (page-colors-foreground u)
  (pick-color (initial-set-tree u "color" answer)))

(tm-widget ((document-colors-picker u) quit)
  (padded
    (refreshable "page-colors"
      (tabs
        (tab (text "Background")
          (padded
            (dynamic (page-colors-background u))))
        (tab (text "Foreground")
          (padded
            (dynamic (page-colors-foreground u))))))
    ======
    (explicit-buttons
      (hlist
        >>>
        ("Reset"
         (initial-default u "bg-color" "color")
         (refresh-now "page-colors"))
        // //
        ("Ok" (quit))))))

(tm-define (open-document-colors)
  (:interactive #t)
  (with u (current-buffer)
    (dialogue-window (document-colors-picker u) noop "Document colors")))
