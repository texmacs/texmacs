
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

(define boot-start (texmacs-time))

;; TODO: scheme file caching using (set! primitive-load ...) and
;; (set! %search-load-path)

;; (debug-enable 'backtrace 'debug)
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
(if (os-mingw?)
    (load "kernel/boot/boot.scm")
    (load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot.scm")))
(inherit-modules (kernel boot compat) (kernel boot abbrevs)
		 (kernel boot debug) (kernel boot srfi)
		 (kernel boot ahash-table) (kernel boot prologue))
(inherit-modules (kernel library base) (kernel library list)
		 (kernel library tree) (kernel library content))
(inherit-modules (kernel regexp regexp-match) (kernel regexp regexp-select))
(inherit-modules (kernel drd drd-rules) (kernel drd drd-query)
		 (kernel drd drd-data))
(inherit-modules (kernel texmacs tm-overload) (kernel texmacs tm-define)
		 (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
		 (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
		 (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
		 (kernel texmacs tm-language)  (kernel texmacs tm-file-system))
(inherit-modules (kernel gui gui-markup)
                 (kernel gui menu-define) (kernel gui menu-widget)
		 (kernel gui kbd-define) (kernel gui kbd-handlers)
		 (kernel gui gui-widget) (kernel gui gui-factory)
		 (kernel gui gui-form))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting utilities\n")
(lazy-define (utils cas cas-out) cas->stree)
(use-modules (utils misc markup-funcs))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting BibTeX style modules\n")
(use-modules (bibtex bib-utils))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting main TeXmacs functionality\n")
(use-modules (texmacs texmacs tm-server) (texmacs texmacs tm-view)
	     (texmacs texmacs tm-files) (texmacs texmacs tm-print))
(use-modules (texmacs keyboard config-kbd))
(lazy-keyboard (texmacs keyboard prefix-kbd) always?)
(lazy-keyboard (texmacs keyboard latex-kbd) always?)
(lazy-menu (texmacs menus file-menu) file-menu go-menu
	   new-file-menu load-menu save-menu print-menu close-menu)
(lazy-menu (texmacs menus edit-menu) edit-menu)
(lazy-menu (texmacs menus view-menu) view-menu)
(lazy-menu (texmacs menus tools-menu) tools-menu)
(lazy-menu (texmacs menus preferences-menu) preferences-menu page-setup-menu)
(use-modules (texmacs menus main-menu))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting generic mode\n")
(lazy-keyboard (generic generic-kbd) always?)
(lazy-menu (generic generic-menu) focus-menu texmacs-focus-icons)
(lazy-menu (generic format-menu) format-menu
	   font-size-menu color-menu horizontal-space-menu
	   transform-menu specific-menu
	   vertical-space-menu indentation-menu line-break-menu
	   page-header-menu page-footer-menu page-numbering-menu
	   page-break-menu page-insertion-menu
	   insert-page-insertion-menu position-float-menu)
(lazy-menu (generic document-menu) document-menu
	   project-menu document-style-menu global-language-menu)
(lazy-menu (generic document-part) document-part-menu project-manage-menu)
(lazy-menu (generic insert-menu) insert-menu texmacs-insert-icons
	   insert-link-menu insert-image-menu insert-animation-menu)
(lazy-define (generic generic-doc) focus-help)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting text mode\n")
(lazy-keyboard (text text-kbd) in-text?)
(lazy-keyboard (text format-text-kbd) in-text?)
(lazy-keyboard (text std-text-kbd) in-std-text?)
(lazy-menu (text format-text-menu) text-format-menu text-format-icons)
(lazy-menu (text text-menu) text-menu text-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting math mode\n")
(lazy-keyboard (math math-kbd) in-math?)
(lazy-menu (math format-math-menu) math-format-menu math-format-icons)
(lazy-menu (math math-menu) math-menu math-icons insert-math-menu
           math-correct-menu semantic-math-preferences-menu
           context-preferences-menu)
(lazy-initialize (math math-menu) (in-math?))
(lazy-define (math math-edit) brackets-refresh)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting programming modes\n")
(lazy-keyboard (prog scheme-edit) in-prog-scheme?)
(lazy-menu (prog format-prog-menu) prog-format-menu prog-format-icons)
(lazy-menu (prog prog-menu) prog-menu prog-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting source mode\n")
(lazy-keyboard (source source-kbd) always?)
(lazy-menu (source source-menu) source-menu source-icons
	   source-transformational-menu source-executable-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting table mode\n")
(lazy-keyboard (table table-kbd) in-table?)
(lazy-menu (table table-menu) insert-table-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting graphics mode\n")
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons)
(lazy-define (graphics graphics-edit)
	     graphics-reset-context graphics-undo-enabled
	     graphics-release-left graphics-release-middle
	     graphics-release-right graphics-start-drag
	     graphics-dragging graphics-end-drag)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting formal languages\n")
(lazy-language (language minimal) minimal)
(lazy-language (language std-math) std-math)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting dynamic features\n")
(lazy-keyboard (dynamic fold-kbd) always?)
(lazy-keyboard (dynamic scripts-kbd) always?)
(lazy-menu (dynamic fold-menu) insert-fold-menu dynamic-menu dynamic-icons)
(lazy-menu (dynamic session-menu) insert-session-menu session-help-icons)
(lazy-menu (dynamic scripts-menu) scripts-eval-menu scripts-plot-menu
	   plugin-eval-menu plugin-eval-toggle-menu plugin-plot-menu)
(lazy-define (dynamic session-edit) scheme-eval)
(lazy-define (dynamic form-edit) form-ref form-set! form-toggle)
(lazy-initialize (dynamic session-menu) (in-session?))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting documentation\n")
(lazy-keyboard (doc tmdoc-kbd) in-tmdoc?)
(lazy-menu (doc help-menu) help-menu)
(lazy-define (doc tmdoc) tmdoc-expand-help tmdoc-expand-help-manual
	     tmdoc-expand-this tmdoc-include)
(lazy-define (doc docgrep) docgrep-in-doc docgrep-in-src docgrep-in-texts)
(lazy-define (doc tmweb) tmweb-convert-dir tmweb-update-dir
             tmweb-interactive-build tmweb-interactive-update)
(define-secure-symbols tmdoc-include)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting converters\n")
(lazy-format (convert rewrite init-rewrite) texmacs scheme verbatim)
(lazy-format (convert tmml init-tmml) tmml)
(lazy-format (convert latex init-latex) latex)
(lazy-format (convert html init-html) html)
(lazy-format (convert bibtex init-bibtex) bibtex)
(lazy-format (convert images init-images)
	     postscript pdf xfig xmgrace svg xpm jpeg ppm gif png pnm)
(lazy-define (convert rewrite init-rewrite) texmacs->verbatim)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity latex-type)
(lazy-define (convert latex textm) textm-finalize)
(lazy-define (convert latex tmtex) tmtex-env-patch)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting remote facilities\n")
(lazy-define (remote tmfs-remote) remote-load remote-save
	     remote-name remote-permission? remote-project-load-by-name)
(lazy-menu (remote remote-menu) remote-menu)
(lazy-menu (remote chat-menu) chat-menu)
(lazy-menu (remote tmfs-menu) remote-file-menu)
(tmfs-handler #t 'load remote-load)
(tmfs-handler #t 'save remote-save)
(tmfs-handler #t 'name remote-name)
(tmfs-handler #t 'permission? remote-permission?)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting linking facilities\n")
(lazy-menu (link link-menu) link-menu)
(lazy-keyboard (link link-kbd) with-linking-tool?)
(lazy-define (link link-edit) create-unique-id)
(lazy-define (link link-navigate) link-active-upwards link-active-ids
	     link-follow-ids)
(lazy-define (link link-extern) get-constellation
	     get-link-locations register-link-locations)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting versioning facilities\n")
(lazy-menu (version version-menu) version-menu)
(lazy-keyboard (version version-kbd) with-versioning-tool?)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting debugging facilities\n")
(lazy-menu (texmacs menus debug-menu) debug-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting plugins\n")
(for-each lazy-plugin-initialize (plugin-list))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting fonts\n")
(use-modules (fonts fonts-ec) (fonts fonts-adobe) (fonts fonts-x)
	     (fonts fonts-math) (fonts fonts-foreign) (fonts fonts-misc)
	     (fonts fonts-composite) (fonts fonts-truetype))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "------------------------------------------------------\n")
(delayed (:idle 10000) (delayed-auto-save))
(texmacs-banner)
;(display "Initialization done\n")
