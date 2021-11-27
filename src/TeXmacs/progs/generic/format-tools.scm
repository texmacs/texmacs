
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-tools.scm
;; DESCRIPTION : Widgets for text, paragraph and page properties
;; COPYRIGHT   : (C) 2013-2021  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-tools)
  (:use (generic format-widgets)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (window-get-env win var mode)
  (or (with-window win
        (cond ((== mode :here)
               (get-env var))
              ((== mode :paragraph)
               (get-env var))
              ((== mode :global)
               (get-init var))
              (else "?")))
      "?"))

(tm-define (window-set-env win var val mode)
  (with-window win
    (cond ((== mode :here)
           (make-with (list var val)))
          ((== mode :paragraph)
           (make-multi-line-with (list var val)))
          ((== mode :global)
           (with-window win (init-env var val))
           (update-menus)))))

(tm-define (window-get-init win var)
  (window-get-env win var :global))

(tm-define (window-set-init win var val)
  (window-set-env win var val :global))

(tm-define (window-defined-init? win var)
  (with-window win
    (style-has? var)))

(tm-define (window-reset-init win . vars)
  (with-window win
    (apply init-default vars)
    (update-menus)))

(tm-define (window-id win)
  (url->string (url-tail win)))

(define tool-key-table (make-ahash-table))

(tm-define (tool-ref key)
  (ahash-ref tool-key-table key))

(tm-define (tool-set key val)
  (ahash-set! tool-key-table key val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (paragraph-basic-tool win mode)
  (refreshable "paragraph tool"
    (aligned
      (item (text "Alignment:")
        (enum (window-set-env win "par-mode" answer mode)
              '("left" "center" "right" "justify")
              (window-get-env win "par-mode" mode) "10em"))
      (assuming (== mode :paragraph)
        (item ====== ======)
        (item (text "Left margin:")
          (enum (window-set-env win "par-left" answer mode)
                (cons-new (window-get-env win "par-left" mode)
                          '("0tab" "1tab" "2tab" ""))
                (window-get-env win "par-left" mode) "10em"))
        (item (text "Right margin:")
          (enum (window-set-env win "par-right" answer mode)
                (cons-new (window-get-env win "par-right" mode)
                          '("0tab" "1tab" "2tab" ""))
                (window-get-env win "par-right" mode) "10em")))
      (item (text "First indentation:")
        (enum (window-set-env win "par-first" answer mode)
              (cons-new (window-get-env win "par-first" mode)
                        '("0tab" "1tab" "-1tab" ""))
              (window-get-env win "par-first" mode) "10em"))
      (item ====== ======)
      (item (text "Interline space:")
        (enum (window-set-env win "par-sep" answer mode)
              (cons-new (window-get-env win "par-sep" mode)
                        '("0fn" "0.2fn" "0.5fn" "1fn" ""))
              (window-get-env win "par-sep" mode) "10em"))
      (item (text "Interparagraph space:")
        (enum (window-set-env win "par-par-sep" answer mode)
              (cons-new (window-get-env win "par-par-sep" mode)
                        '("0fn" "0.3333fn" "0.5fn" "0.6666fn"
                          "1fn" "0.5fns" ""))
              (window-get-env win "par-par-sep" mode) "10em"))
      (item ====== ======)
      (item (text "Number of columns:")
        (enum (begin
                (window-set-env win "par-columns" answer mode)
                (refresh-now "paragraph-tool-columns"))
              '("1" "2" "3" "4" "5" "6")
              (window-get-env win "par-columns" mode) "10em"))
      (item (when (!= (window-get-env win "par-columns" mode) "1")
              (text "Column separation:"))
        (refreshable "paragraph-formatter-columns-sep"
          (when (!= (window-get-env win "par-columns" mode) "1")
            (enum (window-set-env win "par-columns-sep" answer mode)
                  (cons-new (window-get-env win "par-columns-sep" mode)
                            '("1fn" "2fn" "3fn" ""))
                  (window-get-env win "par-columns-sep" mode) "10em"))))
      (assuming (tool-ref (list win mode "advanced paragraph"))
        (item ====== ======)
        (item === ===)
        (item (text "Line breaking:")
          (enum (window-set-env win "par-hyphen" answer mode)
                '("normal" "professional")
                (window-get-env win "par-hyphen" mode) "10em"))
        (item ====== ======)
        (item (text "Extra interline space:")
          (enum (window-set-env win "par-line-sep" answer mode)
                (cons-new (window-get-env win "par-line-sep" mode)
                          '("0fn" "0.025fns" "0.05fns" "0.1fns"
                            "0.2fns" "0.5fns" "1fns" ""))
                (window-get-env win "par-line-sep" mode) "10em"))
        (item (text "Minimal line separation:")
          (enum (window-set-env win "par-ver-sep" answer mode)
                (cons-new (window-get-env win "par-ver-sep" mode)
                          '("0fn" "0.1fn" "0.2fn" "0.5fn" "1fn" ""))
                (window-get-env win "par-ver-sep" mode) "10em"))
        (item (text "Horizontal collapse distance:")
          (enum (window-set-env win "par-hor-sep" answer mode)
                (cons-new (window-get-env win "par-hor-sep" mode)
                          '("0.1fn" "0.2fn" "0.5fn" "1fn"
                            "2fn" "5fn" "10fn" "100fn" ""))
                (window-get-env win "par-hor-sep" mode) "10em"))
        (item ====== ======)
        (item (text "Space stretchability:")
          (enum (window-set-env win "par-flexibility" answer mode)
                (cons-new (window-get-env win "par-flexibility" mode)
                          '("1" "2" "4" "1000" ""))
                (window-get-env win "par-flexibility" mode) "10em"))
        (item (text "CJK spacing:")
          (enum (window-set-env win "par-spacing" answer mode)
                '("plain" "quanjiao" "banjiao" "hangmobanjiao" "kaiming")
                (window-get-env win "par-spacing" mode) "10em"))
        (item ====== ======)
        (item (text "Intercharacter stretching:")
          (enum (window-set-env win "par-kerning-stretch" answer mode)
                (cons-new (window-get-env win "par-kerning-stretch" mode)
                          '("auto" "tolerant"
                            "0" "0.02" "0.05" "0.1" "0.2" "0.5" "1" ""))
                (window-get-env win "par-kerning-stretch" mode) "10em"))
        (item (text "Intercharacter compression:")
          (enum (window-set-env win "par-kerning-reduce" answer mode)
                (cons-new (window-get-env win "par-kerning-reduce" mode)
                          '("auto"
                            "0" "0.01" "0.02" "0.03" "0.05" "0.1" "0.2" ""))
                (window-get-env win "par-kerning-reduce" mode) "10em"))
        (item (text "Character expansion:")
          (enum (window-set-env win "par-expansion" answer mode)
                (cons-new (window-get-env win "par-expansion" mode)
                          '("auto" "tolerant"
                            "0" "0.01" "0.02" "0.05" "0.1" "0.2" ""))
                (window-get-env win "par-expansion" mode) "10em"))
        (item (text "Character contraction:")
          (enum (window-set-env win "par-contraction" answer mode)
                (cons-new (window-get-env win "par-contraction" mode)
                          '("auto" "tolerant"
                            "0" "0.01" "0.02" "0.05" "0.1" "0.2" ""))
                (window-get-env win "par-contraction" mode) "10em"))
        (item ====== ======)
        (item (text "Use margin kerning:")
          (toggle (window-set-env win "par-kerning-margin"
                                  (if answer "true" "false") mode)
                  (== (window-get-env win "par-kerning-margin" mode)
                      "true")))))
    ======
    (division "discrete"
      (hlist
        (assuming (== mode :global)
          ("Restore defaults"
           (apply window-reset-init (cons win paragraph-parameters))))
        >>
        (assuming (not (tool-ref (list win mode "advanced paragraph")))
          ("Show advanced settings"
           (begin
             (tool-set (list win mode "advanced paragraph") #t)
             (refresh-now "paragraph tool")
             (update-menus))))
        (assuming (tool-ref (list win mode "advanced paragraph"))
          ("Hide advanced settings"
           (begin
             (tool-set (list win mode "advanced paragraph") #f)
             (refresh-now "paragraph tool")
             (update-menus))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Page format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (window-get-page-rendering win)
  (or (with-window win
        (get-init-page-rendering))
      "?"))

(define (window-set-page-rendering win s)
  (with-window win
    (init-page-rendering s)))

(define (window-page-size-list win)
  (if (window-defined-init? win "beamer-style")
      (list "16:9" "8:5" "4:3" "5:4" "user")
      (list "a3" "a4" "a5" "b4" "b5" "letter" "legal" "executive" "user")))

(define (window-user-page-size? win)
  (== (window-get-init win "page-type") "user"))

(define (encode-rendering s)
  (cond ((== s "screen") "automatic")
        (else s)))

(define (decode-rendering s)
  (cond ((== s "automatic") "screen")
        (else s)))

(define (encode-crop-marks s)
  (cond ((== s "none") "")
        (else s)))

(define (decode-crop-marks s)
  (cond ((== s "") "none")
        (else s)))

(tm-widget (page-format-tool win)
  (refreshable "page format tool"
    ===
    (aligned
      (item (text "Page rendering:")
        (enum (window-set-page-rendering win (encode-rendering answer))
              '("paper" "papyrus" "screen" "beamer" "book" "panorama")
              (decode-rendering (window-get-page-rendering win)) "10em"))
      (item (text "Page type:")
        (enum (begin
                (window-set-init win "page-type" answer)
                (when (!= answer "user")
                  (window-set-init win "page-width" "auto")
                  (window-set-init win "page-height" "auto"))
                (refresh-now "page format tool"))
              (cons-new (window-get-init win "page-type")
                        (window-page-size-list win))
              (window-get-init win "page-type") "10em"))
      (item (when (window-user-page-size? win) (text "Page width:"))
        (when (window-user-page-size? win)
          (enum (window-set-init win "page-width" answer)
                (list (window-get-init win "page-width") "")
                (window-get-init win "page-width") "10em")))
      (item (when (window-user-page-size? win) (text "Page height:"))
        (when (window-user-page-size? win)
          (enum (window-set-init win "page-height" answer)
                (list (window-get-init win "page-height") "")
                (window-get-init win "page-height") "10em")))
      (item (text "Orientation:")
        (enum (window-set-init win "page-orientation" answer)
              '("portrait" "landscape")
              (window-get-init win "page-orientation") "10em"))
      (item (text "First page:")
        (enum (window-set-init win "page-first" answer)
              (list (window-get-init win "page-first") "")
              (window-get-init win "page-first") "10em"))
      (item (text "Crop marks:")
        (enum (window-set-init win "page-crop-marks"
                               (encode-crop-marks answer))
              '("none" "a3" "a4" "letter")
              (decode-crop-marks (window-get-init win "page-crop-marks"))
              "10em")))
    ===
    (division "discrete"
      (hlist
        >>
        ("Restore defaults"
         (window-reset-init win "page-medium" "page-type" "page-orientation"
                            "page-border" "page-packet" "page-offset"
                            "page-width" "page-height" "page-crop-marks"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global page margins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-margins-tool win)
  (padded
    (aligned
      (meti (hlist // (text "Determine margins from text width"))
        (toggle (begin
                  (window-set-init win "page-width-margin"
                                   (if answer "true" "false"))
                  (refresh-now "page-margin-settings"))
                (== (window-get-init win "page-width-margin") "true")))
      (meti (hlist // (text "Same screen margins as on paper"))
        (toggle (begin
                  (window-set-init win "page-screen-margin"
                                   (if answer "false" "true"))
                  (refresh-now "page-screen-margin-settings"))
                (!= (window-get-init win "page-screen-margin") "true")))))
  (refreshable "page-margin-settings"
    ===
    (division "subtitle"
      (text "Margins on paper"))
    (padded
      (if (!= (window-get-init win "page-width-margin") "true")
          (aligned
            (item (text "Left:")
              (hlist
                (input (window-set-init win "page-odd" answer) "string"
                       (list (window-get-init win "page-odd")) "6em")
                // // (text "(odd pages)") // >>>))
            (item (text "")
              (hlist
                (input (window-set-init win "page-even" answer) "string"
                       (list (window-get-init win "page-odd")) "6em")
                // // (text "(even pages)") >>>))
            (item (text "Right:")
              (hlist
                (input (window-set-init win "page-right" answer) "string"
                       (list (window-get-init win "page-right")) "6em")
                // // (text "(odd pages)") // >>>))
            (item (text "Top:")
              (input (window-set-init win "page-top" answer) "string"
                     (list (window-get-init win "page-top")) "6em"))
            (item (text "Bottom:")
              (input (window-set-init win "page-bot" answer) "string"
                     (list (window-get-init win "page-bot")) "6em"))))
      (if (== (window-get-init win "page-width-margin") "true")
          (aligned
            (item (text "Text width:")
              (input (window-set-init win "par-width" answer) "string"
                     (list (window-get-init win "par-width")) "6em"))
            (item (text "Odd page shift:")
              (input (window-set-init win "page-odd-shift" answer) "string"
                     (list (window-get-init win "page-odd-shift")) "6em"))
            (item (text "Even page shift:")
              (input (window-set-init win "page-even-shift" answer) "string"
                     (list (window-get-init win "page-even-shift")) "6em"))
            (item (text "Top:")
              (input (window-set-init win "page-top" answer) "string"
                     (list (window-get-init win "page-top")) "6em"))
            (item (text "Bottom:")
              (input (window-set-init win "page-bot" answer) "string"
                     (list (window-get-init win "page-bot")) "6em"))))))
  (refreshable "page-screen-margin-settings"
    (assuming (== (window-get-init win "page-screen-margin") "true")
      ===
      (division "subtitle"
        (text "Margins on screen"))
      (padded
        (aligned
          (item (text "Left:")
            (input (window-set-init win "page-screen-left" answer) "string"
                   (list (window-get-init win "page-screen-left")) "6em"))
          (item (text "Right:")
            (input (window-set-init win "page-screen-right" answer) "string"
                   (list (window-get-init win "page-screen-right")) "6em"))
          (item (text "Top:")
            (input (window-set-init win "page-screen-top" answer) "string"
                   (list (window-get-init win "page-screen-top")) "6em"))
          (item (text "Bottom:")
            (input (window-set-init win "page-screen-bot" answer) "string"
                   (list (window-get-init win "page-screen-bot")) "6em"))))))
  (division "discrete"
    (hlist
      >>
      ("Restore defaults"
       (window-reset-init win "page-odd" "page-even" "page-right"
                          "page-top" "page-bot" "par-width"
                          "page-odd-shift" "page-even-shift"
                          "page-screen-left" "page-screen-right"
                          "page-screen-top" "page-screen-bot"
                          "page-width-margin" "page-screen-margin")
       (refresh-now "page-margin-toggles")
       (refresh-now "page-margin-settings")
       (refresh-now "page-screen-margin-settings")))))

(tm-widget (page-margins-tool win)
  (:require (style-has? "std-latex-dtd"))
  (padded
    (text "This style specifies page margins in the TeX way"))
  (refreshable "page-margin-toggles"
    (padded
      (aligned
        (meti (hlist // (text "Same screen margins as on paper"))
          (toggle (begin
                    (window-set-init win "page-screen-margin"
                                     (if answer "false" "true"))
                    (refresh-now "page-screen-margin-settings"))
                  (!= (window-get-init win "page-screen-margin") "true"))))))
  (refreshable "page-tex-hor-margins"
    ===
    (division "subtitle"
      (text "Horizontal margins"))
    (padded
      (aligned
        (item (text "oddsidemargin:")
          (input (window-set-init win "tex-odd-side-margin" answer) "string"
                 (list (window-get-init win "tex-odd-side-margin")) "6em"))
        (item (text "evensidemargin:")
          (input (window-set-init win "tex-even-side-margin" answer) "string"
                 (list (window-get-init win "tex-even-side-margin")) "6em"))
        (item (text "textwidth:")
          (input (window-set-init win "tex-text-width" answer) "string"
                 (list (window-get-init win "tex-text-width")) "6em"))
        (item (text "linewidth:")
          (input (window-set-init win "tex-line-width" answer) "string"
                 (list (window-get-init win "tex-line-width")) "6em"))
        (item (text "columnwidth:")
          (input (window-set-init win "tex-column-width" answer) "string"
                 (list (window-get-init win "tex-column-width")) "6em")))))
  (refreshable "page-tex-ver-margins"
    ===
    (division "subtitle"
      (text "Vertical margins"))
    (padded
      (aligned
        (item (text "topmargin:")
          (input (window-set-init win "tex-top-margin" answer) "string"
                 (list (window-get-init win "tex-top-margin")) "6em"))
        (item (text "headheight:")
          (input (window-set-init win "tex-head-height" answer) "string"
                 (list (window-get-init win "tex-head-height")) "6em"))
        (item (text "headsep:")
          (input (window-set-init win "tex-head-sep" answer) "string"
                 (list (window-get-init win "tex-head-sep")) "6em"))
        (item (text "textheight:")
          (input (window-set-init win "tex-text-height" answer) "string"
                 (list (window-get-init win "tex-text-height")) "6em"))
        (item (text "footskip:")
          (input (window-set-init win "tex-foot-skip" answer) "string"
                 (list (window-get-init win "tex-foot-skip")) "6em")))))
  (refreshable "page-screen-margin-settings"
    (assuming (== (window-get-init win "page-screen-margin") "true")
      ===
      (division "subtitle"
        (text "Margins on screen"))
      (padded
        (aligned
          (item (text "Left:")
            (input (window-set-init win "page-screen-left" answer) "string"
                   (list (window-get-init win "page-screen-left")) "6em"))
          (item (text "Right:")
            (input (window-set-init win "page-screen-right" answer) "string"
                   (list (window-get-init win "page-screen-right")) "6em"))
          (item (text "Top:")
            (input (window-set-init win "page-screen-top" answer) "string"
                   (list (window-get-init win "page-screen-top")) "6em"))
          (item (text "Bottom:")
            (input (window-set-init win "page-screen-bot" answer) "string"
                   (list (window-get-init win "page-screen-bot")) "6em"))))))
  (division "discrete"
    (hlist
      >>
      ("Restore defaults"
       (window-reset-init win "tex-odd-side-margin" "tex-even-side-margin"
                          "tex-text-width" "tex-line-width"
                          "tex-column-width" "tex-top-margin"
                          "tex-head-height" "tex-head-sep"
                          "tex-text-height" "tex-foot-skip"
                          "page-screen-left" "page-screen-right"
                          "page-screen-top" "page-screen-bot"
                          "page-width-margin" "page-screen-margin")
       (refresh-now "page-margin-toggles")
       (refresh-now "page-tex-hor-margins")
       (refresh-now "page-tex-ver-margins")
       (refresh-now "page-screen-margin-settings")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-breaking-tool win)
  (refreshable "page-breaking-settings"
    ===
    (aligned
      (item (text "Page breaking algorithm:")
        (enum (window-set-init win "page-breaking" answer)
              '("sloppy" "professional")
              (window-get-init win "page-breaking") "10em"))
      (item (text "Allowed page height reduction:")
        (enum (window-set-init win "page-shrink" answer)
              (cons-new (window-get-init win "page-shrink")
                        '("0cm" "0.5cm" "1cm" ""))
              (window-get-init win "page-shrink") "10em"))
      (item (text "Allowed page height extension:")
        (enum (window-set-init win "page-extend" answer)
              (cons-new (window-get-init win "page-extend")
                        '("0cm" "0.5cm" "1cm" ""))
              (window-get-init win "page-extend") "10em"))
      (item (text "Vertical space stretchability:")
        (enum (window-set-init win "page-flexibility" answer)
              (cons-new (window-get-init win "page-flexibility")
                        '("0" "0.25" "0.5" "0.75" "1" ""))
              (window-get-init win "page-flexibility") "10em")))
    ===
    (division "discrete"
      (hlist
        >>
        ("Restore defaults"
         (window-reset-init win "page-breaking" "page-shrink"
                            "page-extend" "page-flexibility")
         (refresh-now "page-breaking-settings"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Header & Footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define header-parameters
  (list "page-odd-header" "page-even-header"
        "page-odd-footer" "page-even-footer"))

(define (header-buffer win var)
  (string->url (string-append "tmfs://aux/" var "-" (window-id win))))

(define (get-field-contents u)
  (and-with t (tm->stree (buffer-get-body u))
    (when (tm-func? t 'document 1)
      (set! t (tm-ref t 0)))
    t))

(define (apply-headers-settings win u)
  (with l (list)
    (for (var header-parameters)
      (and-with doc (get-field-contents (header-buffer win var))
        (set! l (cons `(,var ,doc) l))))
    (when (nnull? l)
      (delayed
        (:idle 10)
        (for (x l) (initial-set-tree u (car x) (cadr x)))
        (refresh-window)))))

(define (editing-headers? win)
  (in? (current-buffer)
       (map (lambda (var) (header-buffer win var))
            header-parameters)))

(tm-define synchronize-table (make-ahash-table))

(define (synchronize win var)
  (let* ((key (list "header" win var))
         (val (tm->stree (initial-get-tree (window->buffer win) var)))
         (old (ahash-ref synchronize-table key)))
    (when (and old (!= val old))
      (delayed
        (:idle 1)
        (buffer-set-body (header-buffer win var) `(document ,val))))
    (when (!= val old)
      (ahash-set! synchronize-table key val))
    #t))

(tm-widget (page-headers-tool win)
  (let* ((u (window->buffer win))
         (style (list-remove-duplicates
                 (rcons (get-style-list) "macro-editor"))))
    (for (var header-parameters)
      ======
      (text (eval (parameter-name var)))
      ===
      (cached (string-append "edit-" var) (synchronize win var)
        (resize "400px" "60px"
          (texmacs-input `(document ,(initial-get-tree u var))
                         `(style (tuple ,@style))
                         (header-buffer win var))))))
  ====== ===
  (hlist
    (text "Insert:")
    // //
    ("Tab" (when (editing-headers? win) (make-htab "5mm")))
    // //
    ("Page number" (when (editing-headers? win) (make 'page-the-page)))
    >>>
    ("Restore" (apply window-reset-init (cons win header-parameters)))
    // //
    ("Apply"
     (apply-headers-settings win (window->buffer win))
     (with-window win (update-menus)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool (format-paragraph-tool win)
  (:name "This paragraph format")
  (dynamic (paragraph-basic-tool win :paragraph)))

(tm-tool (document-paragraph-tool win)
  (:name "Global paragraph format")
  (dynamic (paragraph-basic-tool win :global)))

(tm-tool (document-page-tool win)
  (:name "Page format")
  (dynamic (page-format-tool win)))

(tm-tool (document-breaking-tool win)
  (:name "Page breaking")
  (dynamic (page-breaking-tool win)))

(tm-tool (document-margins-tool win)
  (:name "Page margins")
  (dynamic (page-margins-tool win)))

(tm-tool (document-headers-tool win)
  (:name "Page headers and footers")
  (dynamic (page-headers-tool win)))

(tm-widget (texmacs-side-tool win tool)
  (:require (== (car tool) 'sections-tool))
  (division "sections"
    (hlist
      ("File" (noop))
      ("Edit" (noop))
      ("Insert" (noop))
      ("Format" (noop))
      (class "active-section" ("Document" (noop)))
      >>>))
  (division "title"
    (text "Page headers and footers"))
  (centered
    (dynamic (page-headers-tool win))
    ======))

(tm-widget (texmacs-side-tool win tool)
  (:require (== (car tool) 'subsections-tool))
  (division "sections"
    (hlist
      ("File" (noop))
      ("Edit" (noop))
      ("Insert" (noop))
      ("Format" (noop))
      (class "active-section" ("Document" (noop)))
      >>>))
  (division "section-tabs"
    (hlist
      ("Style" (noop))
      ("View" (noop))
      (class "section-active-tab" ("Page" (noop)))
      ("Paragraph" (noop))
      ("Font" (noop))
      ("More" (noop))
      >>>))
  (centered
    (dynamic (page-format-tool win))
    ======))
