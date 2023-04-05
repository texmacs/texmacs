
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texmacs.scm
;; DESCRIPTION : This is the standard TeXmacs initialization file
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand (guile-2
;; we remove all optimizations from the compiler
;; this allows for faster loading times
((@ (system base compile) default-optimization-level) 0))
(else #t))

;(cond-expand (guile-2 (display "Guile-2\n")) (else #t))

(cond-expand (guile-2
  (set! %auto-compilation-options
    '(#:warnings (shadowed-toplevel macro-use-before-definition
                  arity-mismatch format duplicate-case-datum
                  bad-case-datum))))
  (else #t))

(cond-expand (guile-2 (module-export-all! (current-module))) (else #t))

; we import some modules which are standard in versions of Guile before 2.2
(cond-expand
  (guile-2
    (use-modules (ice-9 curried-definitions)))
  (else #t))

; conditional expansion of code via macros
; Guile 2 does not allow to have top-level definitions inside conditional statements
; we go around this at macroexpansion
; note that this macro introduces a binding which can clash with others!!!

(define-macro (tm-cond-expand cond . code)
  `(begin (define-macro (tm-cond-expand-init-TEMP) (if ,cond '(begin ,@code) '(begin #t))) (tm-cond-expand-init-TEMP)))
(export-syntax tm-cond-expand)

; Guile 2 has separate expand and evaluation phases so we have several eval-when in the code
; which can be ignored in previous Guile versions.

(cond-expand (guile-2 #t)
  (else
    (define-macro (eval-when a . b) `(begin ,@b)) (export-syntax eval-when)))

; continue with initialization

(cond ((os-mingw?)
       (debug-set! stack 0))
      ((os-macos?)
       (debug-set! stack 2000000))
      (else
       (debug-set! stack 1000000)))

(define boot-start (texmacs-time))
(define remote-client-list (list))

(define developer-mode?
  (equal? (cpp-get-preference "developer tool" "off") "on"))

(if developer-mode?
   (if (equal? (scheme-dialect) "guile-d")
     (debug-enable 'backtrace)
     (debug-enable 'backtrace 'debug)))

(define (%new-read-hook sym) (noop)) ; for autocompletion

(define-public macro-keywords '(define-macro define-public-macro 
                                tm-define-macro))
(define-public def-keywords
  `(define-public provide-public
    tm-define tm-define-once tm-menu menu-bind tm-widget ,@macro-keywords))

(define old-read read)
(define (new-read port)
  "A redefined reader which stores line number and file name in symbols."
  ;; FIXME: handle overloaded definitions
  (let ((form (old-read port)))
    (if (and (pair? form) (member (car form) def-keywords))
        (let* ((l (source-property form 'line))
               (c (source-property form 'column))
               (f (source-property form 'filename))
               (sym  (if (pair? (cadr form)) (caadr form) (cadr form))))
          (if (symbol? sym) ; Just in case
              (let ((old (or (symbol-property sym 'defs) '()))
                    (new `(,f ,l ,c)))
                (%new-read-hook sym)
; (max) I do not understand the logic of the code below
; since there are macros which do not introduce new definitions
; so I will disable it for the moment.
;               (if (and (member (car form) macro-keywords)
;                        (not (member sym def-keywords)))
;                   (set! def-keywords (cons sym def-keywords)))
                (if (not (member new old))
                    (set-symbol-property! sym 'defs (cons new old)))))))
    form))

(define old-primitive-load primitive-load)
(define (new-primitive-load filename)
  (if (member (scheme-dialect) (list "guile-a" "guile-b"))
      (old-primitive-load filename)
      ;; We explicitly circumvent guile's decision to set the current-reader
      ;; to #f inside ice-9/boot-9.scm, try-module-autoload
      (with-fluids ((current-reader read))
                   (old-primitive-load filename))))

(if developer-mode?
    (begin
      (module-export! (current-module)
                      '(%new-read-hook old-read new-read def-keywords))
      (set! read new-read)
      (module-export! (current-module)
                      '(old-primitive-load new-primitive-load))
      (set! primitive-load new-primitive-load)))

;; TODO: scheme file caching using (set! primitive-load ...) and
;; (set! %search-load-path)

;;(debug-enable 'backtrace 'debug)
;; (define load-indent 0)
;; (define old-primitive-load primitive-load)
;; (define (new-primitive-load . x)
;;   (for-each display (make-list load-indent "  "))
;;   (display "Load ") (apply display x) (display "\n")
;;   (set! load-indent (+ load-indent 1))
;;   (apply old-primitive-load x)
;;   (set! load-indent (- load-indent 1))
;;   (for-each display (make-list load-indent "  "))
;;   (display "Done\n"))
;; (set! primitive-load new-primitive-load)

;(display "Booting TeXmacs kernel functionality\n")

; this boots the main TeXmacs module system facilities

(if (os-mingw?)
    (load "kernel/boot/boot.scm")
    (load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot.scm")))

(cond-expand (guile-2
(export! display write object->string string-replace) ;; silence some warnings
) (else #t))

; now we collect basic functionalities by re-exporting all the public symbols
; as part of the current module

(inherit-modules (kernel boot compat) (kernel boot abbrevs)
                 (kernel boot debug) (kernel boot srfi)
                 (kernel boot ahash-table) (kernel boot prologue))
(inherit-modules (kernel library base) (kernel library list)
                 (kernel library tree) (kernel library content)
                 (kernel library patch))
(inherit-modules (kernel regexp regexp-match) (kernel regexp regexp-select))
(inherit-modules (kernel logic logic-rules) (kernel logic logic-query)
                 (kernel logic logic-data))

(cond-expand (guile-2
(export! ... compose select) ;; silence some warnings
) (else #t))

(inherit-modules (kernel texmacs tm-define)
                 (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
                 (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
                 (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
                 (kernel texmacs tm-language) (kernel texmacs tm-file-system)
                 (kernel texmacs tm-states))
(inherit-modules (kernel gui gui-markup)
                 (kernel gui menu-define) (kernel gui menu-widget)
                 (kernel gui kbd-define)
                 (kernel gui speech-define)
                 (kernel gui kbd-handlers)
                 (kernel gui menu-test)
                 (kernel old-gui old-gui-widget)
                 (kernel old-gui old-gui-factory)
                 (kernel old-gui old-gui-form)
                 (kernel old-gui old-gui-test))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting utilities\n")
(use-modules (utils library cpp-wrap))
(lazy-define (utils library cursor) notify-cursor-moved)
(lazy-define (utils edit variants) make-inline-tag-list make-wrapped-tag-list)
(lazy-define (utils cas cas-out) cas->stree)
(lazy-define (utils plugins plugin-cmd) pre-serialize verbatim-serialize)
(lazy-define (utils test test-convert) delayed-quit
             build-manual build-ref-suite run-test-suite)
(use-modules (utils library smart-table))
(use-modules (utils plugins plugin-convert))
(use-modules (utils misc markup-funcs))
(use-modules (utils misc artwork))
(use-modules (utils handwriting handwriting))
(lazy-tmfs-handler (utils automate auto-tmfs) automate)
(lazy-define (utils automate auto-tmfs) auto-load-help)
(lazy-keyboard (utils automate auto-kbd) in-auto?)
;;FIXME: handle the evaluation phase of the following two lines
(define supports-email? (url-exists-in-path? "mmail"))
(if supports-email? (use-modules (utils email email-tmfs)))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting BibTeX style modules\n")
(use-modules (bibtex bib-utils))
(lazy-define (bibtex bib-complete) current-bib-file citekey-completions)
(lazy-menu (bibtex bib-widgets) open-bibliography-inserter)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting main TeXmacs functionality\n")
(use-modules (texmacs texmacs tm-server) (texmacs texmacs tm-view)
             (texmacs texmacs tm-files) (texmacs texmacs tm-print))
(lazy-define (texmacs texmacs tm-files)
             buffer-missing-style? buffer-set-default-style)
(use-modules (texmacs keyboard config-kbd))
(lazy-keyboard (texmacs keyboard prefix-kbd) always?)
(lazy-keyboard (texmacs keyboard latex-kbd) always?)
(lazy-menu (texmacs menus file-menu) file-menu go-menu
           new-file-menu load-menu save-menu
           print-menu print-menu-inline close-menu)
(lazy-menu (texmacs menus edit-menu) edit-menu)
(lazy-menu (texmacs menus view-menu) view-menu texmacs-bottom-toolbars)
(lazy-menu (texmacs menus tools-menu) tools-menu)
(lazy-menu (texmacs menus preferences-menu) preferences-menu page-setup-menu)
(lazy-menu (texmacs menus preferences-widgets) open-preferences)
(use-modules (texmacs menus main-menu))
(lazy-define (texmacs menus file-menu) recent-file-list recent-directory-list)
(lazy-define (texmacs menus view-menu) set-bottom-bar test-bottom-bar?)
(tm-define (notify-set-attachment name key val) (noop))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting generic mode\n")
(lazy-keyboard (generic generic-kbd) always?)
(lazy-keyboard (generic generic-speech-en) always?)
(lazy-keyboard (generic generic-speech-fr) always?)
(lazy-menu (generic generic-menu) focus-menu texmacs-focus-icons)
(lazy-menu (generic format-menu) format-menu
           font-size-menu color-menu horizontal-space-menu
           transform-menu specific-menu
           text-font-effects-menu text-effects-menu
           vertical-space-menu indentation-menu line-break-menu
           page-header-menu page-footer-menu page-numbering-menu
           page-break-menu)
(lazy-menu (generic document-menu) document-menu
           cite-texmacs-menu cite-texmacs-short-menu
           project-menu document-style-menu global-language-menu)
(lazy-menu (generic document-part)
           preamble-menu document-part-menu project-manage-menu)
(lazy-menu (generic insert-menu) insert-menu texmacs-insert-menu
           texmacs-insert-icons insert-link-menu insert-image-menu)
(lazy-define (generic document-edit) update-document set-document-language
             get-init-page-rendering init-page-rendering)
(lazy-define (generic generic-edit) notify-activated notify-disactivated
             wheel-capture?)
(lazy-define (generic generic-doc) focus-help)
(lazy-define (generic search-widgets) search-toolbar replace-toolbar
             open-search toolbar-search-start interactive-search
             open-replace toolbar-replace-start interactive-replace
             search-next-match)
(lazy-define (generic spell-widgets) spell-toolbar
             open-spell toolbar-spell-start interactive-spell)
(lazy-define (generic format-widgets) open-paragraph-format open-page-format)
(lazy-define (generic pattern-selector) open-pattern-selector
             open-gradient-selector open-background-picture-selector)
(lazy-define (generic document-widgets) open-source-tree-preferences
             open-document-paragraph-format open-document-page-format
             open-document-metadata open-document-colors)
(tm-property (open-search) (:interactive #t))
(tm-property (open-replace) (:interactive #t))
(tm-property (open-paragraph-format) (:interactive #t))
(tm-property (open-page-format) (:interactive #t)
                                (:applicable (not (selection-active?))))
(tm-property (open-source-tree-preferences) (:interactive #t))
(tm-property (open-document-paragraph-format) (:interactive #t))
(tm-property (open-document-page-format) (:interactive #t))
(tm-property (open-document-metadata) (:interactive #t))
(tm-property (open-document-colors) (:interactive #t))
(tm-property (open-pattern-selector cmd w) (:interactive #t))
(tm-property (open-gradient-selector cmd) (:interactive #t))
(tm-property (open-background-picture-selector cmd) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting text mode\n")
(lazy-keyboard (text text-kbd) in-text?)
(lazy-keyboard (text text-speech-en) in-text?)
(lazy-keyboard (text text-speech-fr) in-text?)
(lazy-keyboard (text chinese chinese) in-chinese?)
(lazy-menu (text text-menu) text-format-menu text-format-icons
	   text-menu text-block-menu text-inline-menu
           text-icons text-block-icons text-inline-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting math mode\n")
(lazy-keyboard (math math-kbd) in-math?)
(lazy-keyboard (math math-sem-edit) in-sem-math?)
(lazy-keyboard (math math-speech-en) in-math?)
(lazy-keyboard (math math-adjust-en) in-math?)
(lazy-keyboard (math math-speech-fr) in-math?)
(lazy-keyboard (math math-adjust-fr) in-math?)
(lazy-menu (math math-menu) math-format-menu math-format-icons
	   math-menu math-insert-menu
           math-icons math-insert-icons
           math-correct-menu semantic-math-preferences-menu
           context-preferences-menu insert-math-menu)
(lazy-initialize (math math-menu) (in-math?))
(lazy-define (math math-edit) brackets-refresh)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting programming modes\n")
(lazy-format (prog prog-format) scheme)
(lazy-format (code-format) cpp julia scala java json csv)
(lazy-format (mathemagix-format) mathemagix)
(lazy-format (python-format) python)
(lazy-format (scilab-format) scilab)
(lazy-keyboard (prog prog-kbd) in-prog?)
(lazy-menu (prog prog-menu) prog-format-menu prog-format-icons
	   prog-menu prog-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting source mode\n")
(lazy-keyboard (source source-kbd) always?)
(lazy-menu (source source-menu) source-macros-menu source-menu source-icons
           source-transformational-menu source-executable-menu)
(lazy-define (source macro-edit)
             has-macro-source? edit-macro-source edit-focus-macro-source)
(lazy-menu (source macro-menu) insert-macro-menu)
(lazy-define (source macro-widgets)
             editable-macro? open-macros-editor
	     open-macro-editor create-table-macro
             edit-focus-macro edit-previous-macro)
(lazy-define (source shortcut-edit) init-user-shortcuts has-user-shortcut?)
(lazy-define (source shortcut-widgets) open-shortcuts-editor)
(tm-property (open-macro-editor l mode) (:interactive #t))
(tm-property (create-table-macro l mode) (:interactive #t))
(tm-property (open-macros-editor mode) (:interactive #t))
(tm-property (edit-focus-macro) (:interactive #t))
(tm-property (open-shortcuts-editor . opt) (:interactive #t))
(when (url-exists? "$TEXMACS_HOME_PATH/system/shortcuts.scm")
  (delayed (:idle 100) (init-user-shortcuts)))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting table mode\n")
(lazy-keyboard (table table-kbd) in-table?)
(lazy-menu (table table-menu) insert-table-menu)
(lazy-define (table table-edit) table-resize-notify)
(lazy-define (table table-widgets) open-cell-properties open-table-properties)
(tm-property (open-cell-properties) (:interactive #t))
(tm-property (open-table-properties) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting graphics mode\n")
(lazy-keyboard (graphics graphics-kbd) in-active-graphics? graphics-wheel)
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons
           graphics-focus-icons)
(lazy-define (graphics graphics-object)
             graphics-reset-state graphics-decorations-update)
(lazy-define (graphics graphics-utils) make-graphics)
(lazy-define (graphics graphics-edit)
             graphics-busy?
             graphics-reset-context graphics-undo-enabled
             graphics-release-left graphics-release-middle
             graphics-release-right graphics-start-drag-left
             graphics-dragging-left graphics-end-drag-left)
(lazy-define (graphics graphics-main) graphics-update-proviso
             graphics-get-proviso graphics-set-proviso)
(lazy-define (graphics graphics-markup) arrow-with-text arrow-with-text*)
(define-secure-symbols arrow-with-text arrow-with-text*)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting formal and natural languages\n")
(lazy-language (language minimal) minimal)
(lazy-language (language std-math) std-math)
(lazy-define (language natural) replace)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting educational features\n")
(lazy-keyboard (education edu-kbd) in-edu-text?)
(lazy-menu (education edu-menu) edu-insert-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting dynamic features\n")
(lazy-keyboard (dynamic fold-kbd) always?)
(lazy-keyboard (dynamic scripts-kbd) always?)
(lazy-keyboard (dynamic calc-kbd) always?)
(lazy-menu (dynamic fold-menu) insert-fold-menu dynamic-menu dynamic-icons
           graphics-overlays-menu graphics-screens-menu
           graphics-focus-overlays-menu graphics-focus-overlays-icons)
(lazy-menu (dynamic session-menu) insert-session-menu session-help-icons)
(lazy-menu (dynamic scripts-menu) scripts-eval-menu scripts-plot-menu
           plugin-eval-menu plugin-eval-toggle-menu plugin-plot-menu)
(lazy-menu (dynamic calc-menu) calc-table-menu calc-insert-menu
           calc-icourse-menu)
(lazy-menu (dynamic animate-menu) insert-animation-menu animate-toolbar)
(lazy-define (dynamic fold-edit)
             screens-switch-to dynamic-make-slides overlays-context?)
(lazy-define (dynamic session-edit) scheme-eval)
(lazy-define (dynamic calc-edit) calc-ready? calc-table-renumber)
(lazy-define (dynamic scripts-plot) open-plots-editor)
(lazy-initialize (dynamic session-menu) (in-session?))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting documentation\n")
(lazy-keyboard (doc tmdoc-kbd) in-manual?)
(lazy-keyboard (doc apidoc-kbd) developer-mode?)
(lazy-menu (doc tmdoc-menu) tmdoc-menu tmdoc-icons)
(lazy-menu (doc help-menu) help-menu)
(lazy-define (doc tmdoc) tmdoc-expand-help tmdoc-expand-help-manual
             tmdoc-expand-this tmdoc-include)
(lazy-define (doc docgrep) docgrep-in-doc docgrep-in-src
             docgrep-in-texts docgrep-in-recent)
(lazy-define (doc tmdoc-search) tmdoc-search-style tmdoc-search-tag
             tmdoc-search-parameter tmdoc-search-scheme)
(lazy-define (doc tmweb) youtube-select
             tmweb-convert-dir tmweb-update-dir
             tmweb-convert-dir-keep-texmacs tmweb-update-dir-keep-texmacs
             tmweb-interactive-build tmweb-interactive-update
             open-website-builder)
(lazy-define (doc apidoc) apidoc-all-modules apidoc-all-symbols)
(lazy-menu (doc apidoc-menu) apidoc-menu)
(lazy-tmfs-handler (doc docgrep) grep)
(lazy-tmfs-handler (doc tmdoc) help)
(lazy-tmfs-handler (doc apidoc) apidoc)
(define-secure-symbols tmdoc-include youtube-select)
(tm-property (open-website-builder) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting converters\n")
(lazy-format (convert rewrite init-rewrite) texmacs verbatim)
(lazy-format (convert tmml init-tmml) tmml)
(lazy-format (convert latex init-latex) latex)
(lazy-format (convert html init-html) html)
(lazy-format (convert bibtex init-bibtex) bibtex)
(lazy-format (convert images init-images)
             postscript pdf xfig xmgrace svg xpm jpeg ppm gif png pnm)
(lazy-define (convert images tmimage)
             export-selection-as-graphics clipboard-copy-image)
(lazy-define (convert rewrite init-rewrite) texmacs->code texmacs->verbatim)
(lazy-define (convert html tmhtml) ext-tmhtml-eqnarray*)
(define-secure-symbols ext-tmhtml-eqnarray*)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity latex-type)
(lazy-define (convert latex tmtex) tmtex-env-patch)
(lazy-define (convert latex latex-tools) latex-set-virtual-packages
             latex-has-style? latex-has-package?
             latex-has-texmacs-style? latex-has-texmacs-package?)
(lazy-menu (convert latex tmtex-widgets) tmtex-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting partial document facilities\n")
(lazy-define (part part-shared) buffer-initialize buffer-notify)
(lazy-menu (part part-menu) document-master-menu)
(lazy-tmfs-handler (part part-tmfs) part)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting database facilities\n")
(lazy-define (database db-widget) open-db-chooser)
(lazy-define (database db-menu) db-show-toolbar)
(lazy-define (database db-convert) db-url?)
(lazy-define (database bib-db) zealous-bib-import zealous-bib-export)
(lazy-define (database bib-manage)
             bib-import-bibtex bib-compile bib-attach open-bib-chooser)
(lazy-define (database bib-local) open-biblio)
(lazy-menu (database db-menu) db-menu db-toolbar)
(lazy-tmfs-handler (database db-tmfs) db)
(lazy-keyboard (database bib-kbd) in-bib?)
(tm-property (open-biblio) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting security tools\n")
(lazy-define (security wallet wallet-menu) expand-with-wallet)
(tm-define-macro (with-wallet . body) (expand-with-wallet body))
(lazy-define (security wallet wallet-base)
	     supports-wallet? wallet-initialized?
	     wallet-on? wallet-off? wallet-get)
(lazy-menu (security wallet wallet-menu) wallet-preferences-widget)
(lazy-define (security gpg gpg-edit) tree-export-encrypted
	     tm-gpg-dialogue-passphrase-decrypt-buffer)
(lazy-define (security gpg gpg-widgets) open-gpg-key-manager)
(lazy-define (security gpg gpg-base) supports-gpg?)
(lazy-menu (security gpg gpg-menu) gpg-menu document-encryption-menu)
(lazy-menu (security gpg gpg-widgets) gpg-preferences-widget)

;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting remote facilities\n")
(lazy-define (client client-tmfs) remote-home-directory)
(lazy-menu (server server-menu) start-server-menu server-menu)
(lazy-menu (client client-menu) start-client-menu client-menu
           remote-menu remote-icons)
(lazy-tmfs-handler (client client-tmfs) remote-file)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting linking facilities\n")
(lazy-menu (link link-menu) link-menu)
(lazy-keyboard (link link-kbd) with-linking-tool?)
(lazy-define (link link-edit) create-unique-id)
(lazy-define (link link-navigate) link-active-upwards link-active-ids
             link-follow-ids)
(lazy-define (link link-extern) get-constellation
             get-link-locations register-link-locations)
(lazy-menu (link ref-menu) ref-menu)
(lazy-define (link ref-edit) preview-reference)
(define-secure-symbols preview-reference)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting versioning facilities\n")
(lazy-menu (version version-menu) version-menu)
(lazy-keyboard (version version-kbd) with-versioning-tool?)
(lazy-define (version version-tmfs) update-buffer commit-buffer)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting debugging and developer facilities\n")
(lazy-menu (debug debug-menu) debug-menu)
(lazy-menu (texmacs menus developer-menu) developer-menu)
(lazy-define (debug debug-widgets) notify-debug-message
             open-debug-console open-error-messages)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting editing modes for various special styles\n")
(lazy-menu (various poster-menu) poster-block-menu)
(lazy-menu (various theme-menu) basic-theme-menu)
(lazy-define (various theme-edit) current-basic-theme)
(lazy-define (various theme-menu) basic-theme-name)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting plugins\n")
(for-each lazy-plugin-initialize (plugin-list))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting fonts\n")
(use-modules (fonts fonts-ec) (fonts fonts-adobe) (fonts fonts-x)
             (fonts fonts-math) (fonts fonts-foreign) (fonts fonts-misc)
             (fonts fonts-composite) (fonts fonts-truetype))
(lazy-define (fonts font-old-menu)
	     text-font-menu math-font-menu prog-font-menu)
(lazy-define (fonts font-new-widgets)
             open-font-selector open-document-font-selector
             open-document-other-font-selector)
(tm-property (open-font-selector) (:interactive #t))
(tm-property (open-document-font-selector) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting regression testing\n")
(lazy-define (check check-master) check-all run-checks run-all-tests)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting autoupdater\n")
(when (updater-supported?) 
  (use-modules (utils misc updater))
  (delayed (:idle 2000) (updater-initialize)))
(display* "time: " (- (texmacs-time) boot-start) "\n")
(display* "memory: " (texmacs-memory) " bytes\n")

(display "------------------------------------------------------\n")
(delayed (:idle 10000) (autosave-delayed))
(texmacs-banner)
(display "Initialization done\n")
