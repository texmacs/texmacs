
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-widgets.scm
;; DESCRIPTION : Widgets for text, paragraph and page properties
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-widgets)
  (:use (generic format-menu)
        (generic document-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-env-table l)
  (with t (make-ahash-table)
    (for (x l)
      (ahash-set! t x (get-env x)))
    t))

(tm-define (get-init-table l)
  (with t (make-ahash-table)
    (for (x l)
      (ahash-set! t x (get-init x)))
    t))

(tm-define (ahash-table-changes old new)
  (with diff (make-ahash-table)
    (for (key (map car (ahash-table->list new)))
      (when (!= (ahash-ref new key) (ahash-ref old key))
        (ahash-set! diff key (ahash-ref new key))))
    diff))

(define (differences-list old new)
  (with diff (ahash-table-changes old new)
    (assoc->list (ahash-table->list diff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define paragraph-parameters
  (list "par-mode" "par-flexibility" "par-hyphen" "par-spacing"
        "par-kerning-stretch" "par-kerning-reduce" "par-expansion"
        "par-contraction" "par-kerning-margin" "par-width"
        "par-left" "par-right" "par-first" "par-no-first"
        "par-sep" "par-hor-sep" "par-ver-sep" "par-line-sep" "par-par-sep"
        "par-fnote-sep" "par-columns" "par-columns-sep"))

(tm-widget (paragraph-formatter-basic old new flag?)
  (aligned
    (item (text "Alignment:")
      (enum (ahash-set! new "par-mode" answer)
            '("left" "center" "right" "justify")
            (ahash-ref new "par-mode") "10em"))
    (assuming (not flag?)
      (item ====== ======)
      (item (text "Left margin:")
        (enum (ahash-set! new "par-left" answer)
              (cons-new (ahash-ref new "par-left")
                        '("0tab" "1tab" "2tab" ""))
              (ahash-ref new "par-left") "10em"))
      (item (text "Right margin:")
        (enum (ahash-set! new "par-right" answer)
              (cons-new (ahash-ref new "par-right")
                        '("0tab" "1tab" "2tab" ""))
              (ahash-ref new "par-right") "10em")))
    (item (text "First indentation:")
      (enum (ahash-set! new "par-first" answer)
            (cons-new (ahash-ref new "par-first") '("0tab" "1tab" "-1tab" ""))
            (ahash-ref new "par-first") "10em"))
    (item ====== ======)
    (item (text "Interline space:")
      (enum (ahash-set! new "par-sep" answer)
            (cons-new (ahash-ref new "par-sep")
                      '("0fn" "0.2fn" "0.5fn" "1fn" ""))
            (ahash-ref new "par-sep") "10em"))
    (item (text "Interparagraph space:")
      (enum (ahash-set! new "par-par-sep" answer)
            (cons-new (ahash-ref new "par-par-sep")
                      '("0fn" "0.3333fn" "0.5fn" "0.6666fn" "1fn" "0.5fns" ""))
            (ahash-ref new "par-par-sep") "10em"))
    (item ====== ======)
    (item (text "Number of columns:")
      (enum (begin
              (ahash-set! new "par-columns" answer)
              (refresh-now "paragraph-formatter-columns-sep"))
            '("1" "2" "3" "4" "5" "6")
            (ahash-ref new "par-columns") "10em"))
    (item (when (!= (ahash-ref new "par-columns") "1")
            (text "Columns separation:"))
      (refreshable "paragraph-formatter-columns-sep"
        (when (!= (ahash-ref new "par-columns") "1")
          (enum (ahash-set! new "par-columns-sep" answer)
                (cons-new (ahash-ref new "par-columns-sep")
                          '("1fn" "2fn" "3fn" ""))
                (ahash-ref new "par-columns-sep") "10em"))))))

(tm-widget (paragraph-formatter-advanced old new)
  (aligned
    (item (text "Line breaking:")
      (enum (ahash-set! new "par-hyphen" answer)
            '("normal" "professional")
            (ahash-ref new "par-hyphen") "10em"))
    (item ====== ======)
    (item (text "Extra interline space:")
      (enum (ahash-set! new "par-line-sep" answer)
            (cons-new (ahash-ref new "par-line-sep")
                      '("0fn" "0.025fns" "0.05fns" "0.1fns"
                        "0.2fns" "0.5fns" "1fns" ""))
            (ahash-ref new "par-line-sep") "10em"))
    (item (text "Minimal line separation:")
      (enum (ahash-set! new "par-ver-sep" answer)
            (cons-new (ahash-ref new "par-ver-sep")
                      '("0fn" "0.1fn" "0.2fn" "0.5fn" "1fn" ""))
            (ahash-ref new "par-ver-sep") "10em"))
    (item (text "Horizontal collapse distance:")
      (enum (ahash-set! new "par-hor-sep" answer)
            (cons-new (ahash-ref new "par-hor-sep")
                      '("0.1fn" "0.2fn" "0.5fn" "1fn"
                        "2fn" "5fn" "10fn" "100fn" ""))
            (ahash-ref new "par-hor-sep") "10em"))
    (item ====== ======)
    (item (text "Space stretchability:")
      (enum (ahash-set! new "par-flexibility" answer)
            (cons-new (ahash-ref new "par-flexibility")
                      '("1" "2" "4" "1000" ""))
            (ahash-ref new "par-flexibility") "10em"))
    (item (text "CJK spacing:")
      (enum (ahash-set! new "par-spacing" answer)
            '("plain" "quanjiao" "banjiao" "hangmobanjiao" "kaiming")
            (ahash-ref new "par-spacing") "10em"))
    (item ====== ======)
    (item (text "Intercharacter stretching:")
      (enum (ahash-set! new "par-kerning-stretch" answer)
            (cons-new (ahash-ref new "par-kerning-stretch")
                      '("auto" "tolerant"
                        "0" "0.02" "0.05" "0.1" "0.2" "0.5" "1" ""))
            (ahash-ref new "par-kerning-stretch") "10em"))
    (item (text "Intercharacter compression:")
      (enum (ahash-set! new "par-kerning-reduce" answer)
            (cons-new (ahash-ref new "par-kerning-reduce")
                      '("auto"
                        "0" "0.01" "0.02" "0.03" "0.05" "0.1" "0.2" ""))
            (ahash-ref new "par-kerning-reduce") "10em"))
    (item (text "Character expansion:")
      (enum (ahash-set! new "par-expansion" answer)
            (cons-new (ahash-ref new "par-expansion")
                      '("auto" "tolerant"
                        "0" "0.01" "0.02" "0.05" "0.1" "0.2" ""))
            (ahash-ref new "par-expansion") "10em"))
    (item (text "Character contraction:")
      (enum (ahash-set! new "par-contraction" answer)
            (cons-new (ahash-ref new "par-contraction")
                      '("auto" "tolerant"
                        "0" "0.01" "0.02" "0.05" "0.1" "0.2" ""))
            (ahash-ref new "par-contraction") "10em")))
  ======
  (centered
    (aligned
      (meti (hlist // (text "Use margin kerning (protrusion)"))
        (toggle (ahash-set! new "par-kerning-margin" (if answer "true" "false"))
                (== (ahash-ref new "par-kerning-margin") "true"))))))

(tm-widget ((paragraph-formatter old new fun u flag?) quit)
  (padded
    (refreshable "paragraph-formatter"
      (tabs
        (tab (text "Basic")
          (padded
            (dynamic (paragraph-formatter-basic old new flag?))))
        (tab (text "Advanced")
          (padded
            (dynamic (paragraph-formatter-advanced old new))))))
    === ===
    (explicit-buttons
      (hlist
        >>>
        (if flag?
            ("Reset"
             (apply init-default paragraph-parameters)
             (with t (get-init-table paragraph-parameters)
               (for (key (map car (ahash-table->list t)))
                 (ahash-set! old key (ahash-ref t key))
                 (ahash-set! new key (ahash-ref t key))))
             (refresh-now "paragraph-formatter"))
            // //)
        ("Apply"
         (when (== u (current-buffer))
           (fun (differences-list old new))
           (for (key (map car (ahash-table->list new)))
             (ahash-set! old key (ahash-ref new key)))))
        // //
        ("Ok"
         (when (== u (current-buffer))
           (fun (differences-list old new)))
         (quit))))))

(tm-define (open-paragraph-format)
  (:interactive #t)
  (let* ((old (get-env-table paragraph-parameters))
         (new (get-env-table paragraph-parameters))
         (u   (current-buffer)))
    (dialogue-window (paragraph-formatter old new make-multi-line-with u #f)
                     noop "Paragraph format")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-field-contents u)
  (and-with t (tm->stree (buffer-get-body u))
    (when (tm-func? t 'document 1)
      (set! t (tm-ref t 0)))
    (and (!= t '(unchanged)) t)))

(define (apply-page-settings u settings)
  (with l (list)
    (and-with nr (ahash-ref settings 'page-nr)
      (set! l (cons `(assign "page-nr" ,nr) l)))
    (and-with sty (ahash-ref settings 'page-the-page)
      (when (!= sty "unchanged")
        (with body (cond ((== sty "normal") `(value "page-nr"))
                         ((== sty "roman") `(number (value "page-nr") "roman"))
                         ((== sty "Roman") `(number (value "page-nr") "Roman"))
                         (else `(value "page-nr")))
          (set! l (cons `(assign "page-the-page" (macro ,body)) l)))))
    (and-with doc (get-field-contents "tmfs://aux/this-page-header")
      (set! l (cons `(set-this-page-header ,doc) l)))
    (and-with doc (get-field-contents "tmfs://aux/this-page-footer")
      (set! l (cons `(set-this-page-footer ,doc) l)))
    (and-with bg (ahash-ref settings "page-this-bg-color")
      (set! l (cons `(assign "page-this-bg-color" ,bg) l)))
    (when (nnull? l)
      (with-buffer u
        (for (x l) (insert x))
        (refresh-window)))))

(define (header-buffer)
  (string->url "tmfs://aux/this-page-header"))

(define (footer-buffer)
  (string->url "tmfs://aux/this-page-footer"))

(define (editing-headers?)
  (in? (current-buffer) (list (header-buffer) (footer-buffer))))

(define ((set-page-pattern settings) name)
  (when (pair? name) (set! name (car name)))
  (ahash-set! settings "page-this-bg-color" (tm-pattern name "" "")))

(define (page-set-background settings what)
  (let* ((var "page-this-bg-color")
         (old (ahash-ref settings var))
         (setter (lambda (c) (ahash-set! settings var c))))
    (cond ((== what "unchanged")
           (ahash-remove! settings var))
          ((== what "color")
           (interactive-color setter (if old (list old) (list))))
          ((== what "pattern")
           (open-pattern-selector setter "1cm"))
          ((== what "picture")
           (open-picture-selector setter)))))

(tm-widget ((page-formatter u style settings) quit)
  (padded
    (centered
      (aligned
        (item (text "This page number:")
          (enum (ahash-set! settings 'page-nr answer)
                '("unchanged" "")
                "unchanged" "10em"))
        (item (text "Page number rendering:")
          (enum (ahash-set! settings 'page-the-page answer)
                '("unchanged" "normal" "roman" "Roman")
                "unchanged" "10em"))
        (item (text "Page background:")
          (enum (page-set-background settings answer)
                '("unchanged" "color" "pattern" "picture")
                "unchanged" "10em"))))
    ======
    (bold (text "This page header"))
    ===
    (resize "600px" "60px"
      (texmacs-input `(document (unchanged))
                     `(style (tuple ,@style)) (header-buffer)))
    === ===
    (bold (text "This page footer"))
    ===
    (resize "600px" "60px"
      (texmacs-input `(document (unchanged))
                     `(style (tuple ,@style)) (footer-buffer)))
    ======
    (explicit-buttons
      (hlist
        (text "Insert:")
        // //
        ("Tab" (when (editing-headers?) (make-htab "5mm")))
        // //
        ("Page number" (when (editing-headers?) (insert '(page-number))))
        >>>
        ("Ok" (apply-page-settings u settings) (quit))))))

(tm-define (open-page-format)
  (:interactive #t)
  (let* ((u  (current-buffer))
         (st (list-remove-duplicates (rcons (get-style-list) "macro-editor")))
         (t  (make-ahash-table)))
    (dialogue-window (page-formatter u st t)
                     noop "Page format"
                     (header-buffer) (footer-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selector / accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-picture? #f)
(define global-pattern-color `(pattern "neutral-pattern.png" "1cm" "100@"))

(define (set-color col)
  (set! global-pattern-color col)
  (refresh-now "pattern-sample")
  (refresh-now "pattern-options"))

(define (get-color)
  global-pattern-color)

(define (set-name name)
  (with p (url->unix "$TEXMACS_PATH/misc/patterns")
    (when (string-starts? name p)
      (set! name (url->unix (url-delta (url-append (unix->url p) "dummy")
                                       (unix->url name)))))
    (with col (get-color)
      (set-color `(pattern ,name ,@(cddr col))))))

(define (get-name)
  (cadr (get-color)))

(define (set-width w)
  (with col (get-color)
    (set-color `(pattern ,(cadr col) ,w ,@(cdddr col)))))

(define (get-width)
  (caddr (get-color)))

(define (set-height h)
  (with col (get-color)
    (set-color `(pattern ,(cadr col) ,(caddr col) ,h ,@(cddddr col)))))

(define (get-height)
  (cadddr (get-color)))

(define (set-size s)
  (cond ((== s "Fit") (set-width "100%") (set-height "100%"))
        ((== s "Fit to width") (set-width "100%") (set-height "100@"))
        ((== s "Fit to height") (set-width "100@") (set-height "100%"))))

(define (get-size)
  (cond ((and (== (get-width) "100%") (== (get-height) "100%")) "Fit")
        ((== (get-width) "100%") "Fit to width")
        ((== (get-height) "100%") "Fit to height")
        (else "Fit")))

(define (reset-effect eff kind)
  (cond ((or (npair? eff) (npair? (cdr eff))) eff)
        ((== (car eff) kind) (reset-effect (cadr eff) kind))
        (else (cons* (car eff) (reset-effect (cadr eff) kind) (cddr eff)))))

(define (assign-effect eff kind args)
  (cons* kind eff args))

(define (set-effect* tail apply? kind args)
  (if (null? tail) (set! tail (list "0")))
  (with res (reset-effect (car tail) kind)
    (with new (if apply? (assign-effect res kind args) res)
      (if (== new "0") (list) (list new)))))

(define (set-effect kind apply? . args)
  (with col (get-color)
    (set-color `(pattern ,(cadr col) ,(caddr col) ,(cadddr col)
                         ,@(set-effect* (cddddr col) apply? kind args)))))

(define (get-effect* eff kind)
  (cond ((or (npair? eff) (npair? (cdr eff))) #f)
        ((== (car eff) kind) (cddr eff))
        (else (get-effect* (cadr eff) kind))))

(define (get-effect kind)
  (with col (get-color)
    (with eff (and (nnull? (cddddr col)) (car (cddddr col)))
      (get-effect* eff kind))))

(define (set-recolor recol)
  (set-effect 'eff-recolor (nnot recol) recol))

(define (get-recolor)
  (and-with opts (get-effect 'eff-recolor)
    (car opts)))

(define (set-blur r)
  (set-effect 'eff-blur (nnot r) `(eff-gaussian ,r)))

(define (get-blur)
  (and-with opts (get-effect 'eff-blur)
    (and (tm-func? (car opts) 'eff-gaussian)
         (tm-ref (car opts) 0))))

(define (normalize-color col)
  (if (tm-func? col 'pattern)
      (apply tm-pattern (cdr col))
      col))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (pattern-name-selector)
  (let* ((name (unix->url (get-name)))
         (base "$TEXMACS_PATH/misc/patterns/neutral-pattern.png")
         (curr (url-relative base name))
         (setter (lambda (c)
                   (when (and (pair? c) (url? (car c)))
                     (set-name (url->unix (car c)))))))
    (hlist
      (enum (set-name (url->unix answer))
            (list (url->system name) "")
            (url->system name) "15em")
      // // //
      ((icon "tm_find.xpm")
       (cond ((not global-picture?)
              (choose-file setter "Background pattern" "image" "" curr))
             ((url-rooted? (unix->url (get-name)))
              (choose-file setter "Background picture" "image" "" curr))
             (else
              (choose-file setter "Background picture" "image"))))
      >>)))

(tm-widget (pattern-recolor-options)
  (with recol (get-recolor)
    (hlist
      (when recol
        (enum (set-recolor answer)
              (list (or recol "") "black" "white" "red" "green" "blue" "")
              (or recol "") "15em"))
      // // //
      (toggle (set-recolor (and answer "black"))
              (nnot (get-recolor)))
      // // //
      ((icon "tm_color.xpm")
       (interactive-color set-recolor (list (or recol ""))))
      >>)))

(tm-widget (pattern-blur-options)
  (with blur (get-blur)
    (hlist
      (when blur
        (enum (set-blur answer)
              (list (or blur "") "0.2pt" "0.5pt" "1pt" "2pt" "5px" "")
              (or blur "") "15em"))
      // // //
      (toggle (set-blur (and answer "1pt"))
              (nnot (get-blur)))
      >>)))

(tm-widget ((pattern-selector u) cmd)
  (padded
    (hlist
      (vlist
        (refreshable "pattern-sample"
          (resize "600px" "450px"
            (texmacs-output `(document
                               (block
                                (tformat
                                 (cwith "1" "1" "1" "1" "cell-width" "590px")
                                 (cwith "1" "1" "1" "1" "cell-height" "440px")
                                 (cwith "1" "1" "1" "1" "cell-vmode" "exact")
                                 (cwith "1" "1" "1" "1" "cell-background"
                                        ,(get-color))
                                 (table (row (cell ""))))))
                            `(style (tuple "generic"))))))
      // // //
      (explicit-buttons
        (vlist
          (refreshable "pattern-options"
            (assuming (not global-picture?)
              (aligned
                (item (text "Name:")
                  (link pattern-name-selector))
                (item (text "Width:")
                  (hlist
                    (enum (set-width answer)
                          (list (get-width) "100%" "100@" "1cm" "")
                          (get-width) "15em") >>))
                (item (text "Height:")
                  (hlist
                    (enum (set-height answer)
                          (list (get-height) "100%" "100@" "1cm" "")
                          (get-height) "15em") >>))
                (item (text "Recolor:")
                  (link pattern-recolor-options))
                ;;(item (text "Blur:")
                ;;  (link pattern-blur-options))
                ))
            (assuming global-picture?
              (aligned
                (item (text "Name:")
                  (link pattern-name-selector))
                (item (text "Size:")
                  (hlist
                    (enum (set-size answer)
                          (list "Fit" "Fit to width" "Fit to height")
                          (get-size) "15em") >>))
                (item (text "Recolor:")
                  (link pattern-recolor-options)))))
          ======
          (glue #f #t 0 0))))
    ======
    (explicit-buttons
      (hlist
        >>>
        ("Ok" (cmd (normalize-color (get-color))))))))

(tm-define (open-pattern-selector cmd w)
  (:interactive #t)
  (when (or global-picture? (== (get-name) "neutral-pattern.png"))
    (set! global-picture? #f)
    (set! global-pattern-color `(pattern "neutral-pattern.png" ,w "100@")))
  (with u (current-buffer)
    (dialogue-window (pattern-selector u) cmd "Pattern selector")))

(tm-define (open-picture-selector cmd)
  (:interactive #t)
  (when (or (not global-picture?) (== (get-name) "neutral-pattern.png"))
    (set! global-picture? #t)
    (set! global-pattern-color `(pattern "neutral-pattern.png" "100%" "100%")))
  (with u (current-buffer)
    (dialogue-window (pattern-selector u) cmd "Background picture selector")))
