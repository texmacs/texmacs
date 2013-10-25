
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
;; Document -> Paragraph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-document-paragraph-format)
  (:interactive #t)
  (let* ((old (get-init-table paragraph-parameters))
         (new (get-init-table paragraph-parameters))
         (u   (current-buffer)))
    (set! paragraph-cur-settings new)
    (dialogue-window (paragraph-formatter old new init-multi u)
                     noop "Document paragraph format")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-formatter-format u quit)
  (padded
    (glue #t #t 500 300)
    === ===
    (explicit-buttons
      (hlist
        >>>
        ("Cancel" (noop))
        // //
        ("Ok" (quit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Margins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-formatter-margins u quit)
  (padded
    (glue #t #t 500 300)
    === ===
    (explicit-buttons
      (hlist
        >>>
        ("Cancel" (noop))
        // //
        ("Ok" (quit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (page-formatter-breaking u quit)
  (padded
    (refreshable "page-breaking-settings"
      (aligned
        (item (text "Page breaking algorithm:")
          (enum (init-env "page-breaking" answer)
                '("sloppy" "medium" "professional")
                (get-init "page-breaking") "10em"))
        (item (text "Allowed page height reduction:")
          (enum (init-env "page-shrink" answer)
                (cons-new (get-init "page-shrink") '("0cm" "0.5cm" "1cm" ""))
                (get-init "page-shrink") "10em"))
        (item (text "Allowed page height extension:")
          (enum (init-env "page-extend" answer)
                (cons-new (get-init "page-extend") '("0cm" "0.5cm" "1cm" ""))
                (get-init "page-extend") "10em"))
        (item (text "Vertical space stretchability:")
          (enum (init-env "page-flexibility" answer)
                (cons-new (get-init "page-flexibility")
                          '("0" "0.25" "0.5" "0.75" "1" ""))
                (get-init "page-flexibility") "10em"))))
    === ===
    (explicit-buttons
      (hlist
        ("Defaults" (begin
                      (init-default "page-breaking" "page-shrink"
                                    "page-extend" "page-flexibility")
                      (refresh-now "page-breaking-settings")))
        >>>
        ("Ok" (quit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document -> Page / Headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define header-parameters
  (list "page-odd-header" "page-even-header"
        "page-odd-footer" "page-even-footer"))

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
        (when (== (current-buffer) u)
          (for (x l) (init-env-tree (car x) (cadr x)))
          (refresh-window))))))

(define (editing-headers?)
  (in? (current-buffer)
       (map (lambda (x) (string->url (string-append "tmfs://aux/" x)))
            header-parameters)))

(tm-widget (page-formatter-headers u style quit)
  (padded
    (for (var header-parameters)
      (bold (text (eval (parameter-name var))))
      ===
      (resize "600px" "60px"
        (texmacs-input `(document ,(get-init-tree var))
                       `(style (tuple ,@style))
                       (string-append "tmfs://aux/" var)))
      === ===)
    === ===
    (explicit-buttons
      (hlist
        (text "Insert:")
        // //
        ("Tab" (when (editing-headers?) (make-htab "5mm")))
        // //
        ("Page number" (when (editing-headers?) (make 'page-the-page)))
        >>>
        ("Ok" (apply-headers-settings u) (quit))))))

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
    (dialogue-window (document-page-formatter u st)
                     noop "Document page format")))
