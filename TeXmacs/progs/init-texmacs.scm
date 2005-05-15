
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texmacs.scm
;; DESCRIPTION : This is the standard TeXmacs initialization file
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boot-start (texmacs-time))

;(display "Booting TeXmacs kernel functionality\n")
(load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot.scm"))
(inherit-modules (kernel boot compat) (kernel boot abbrevs)
		 (kernel boot debug) (kernel boot srfi)
		 (kernel boot ahash-table) (kernel boot prologue)
		 (kernel boot content))
(inherit-modules (kernel regexp regexp-match) (kernel regexp regexp-select))
(inherit-modules (kernel drd drd-rules) (kernel drd drd-query)
		 (kernel drd drd-data))
(inherit-modules (kernel texmacs tm-overload) (kernel texmacs tm-define)
		 (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
		 (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
		 (kernel texmacs tm-convert))
(inherit-modules (kernel gui menu-define) (kernel gui menu-widget)
		 (kernel gui kbd-define))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting utilities\n")
(inherit-modules (utils library base) (utils library list)
		 (utils library tree))
(inherit-modules (utils misc misc-funcs) (utils misc markup-funcs))
(use-modules (utils plugins plugin-cmd))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting main TeXmacs functionality\n")
(use-modules (texmacs texmacs tm-server) (texmacs texmacs tm-view)
	     (texmacs texmacs tm-print))
(lazy-define (texmacs texmacs tm-files) delayed-auto-save)
(use-modules (texmacs keyboard config-kbd))
(lazy-keyboard (texmacs keyboard wildcards-kbd) always?)
(if (like-emacs?) (lazy-keyboard (texmacs keyboard emacs-kbd) always?))
(if (like-windows?) (lazy-keyboard (texmacs keyboard windows-kbd) always?))
(lazy-keyboard (texmacs keyboard latex-kbd) always?)
(lazy-keyboard (texmacs keyboard texmacs-kbd) always?)
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
(lazy-menu (generic format-menu) format-menu font-size-menu color-menu)
(lazy-menu (generic document-menu)
	   document-menu project-menu document-style-menu global-language-menu)
(lazy-menu (generic insert-menu)
	   insert-menu insert-link-menu insert-image-menu
	   insert-page-insertion-menu position-float-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting source mode\n")
(lazy-keyboard (source source-kbd) always?)
(lazy-menu (source source-menu) source-menu source-icons
	   source-transformational-menu source-executable-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting text mode\n")
(lazy-keyboard (text text-kbd) in-text?)
(lazy-keyboard (text format-text-kbd) in-text?)
(lazy-keyboard (text std-text-kbd) in-std-text?)
(lazy-menu (text format-text-menu) text-format-menu text-format-icons)
(lazy-menu (text text-menu) text-menu text-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting table mode\n")
(lazy-keyboard (table table-kbd) in-table?)
(lazy-menu (table table-menu) table-menu table-icons insert-table-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting math mode\n")
(lazy-keyboard (math math-kbd) in-math?)
(lazy-menu (math format-math-menu) math-format-menu math-format-icons)
(lazy-menu (math math-menu) math-menu math-icons insert-math-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting graphics mode\n")
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons)
(lazy-define (graphics graphics-edit)
	     graphics-reset-context graphics-undo-enabled
	     graphics-insert-point graphics-remove-point
	     graphics-last-point graphics-start-drag
	     graphics-dragging graphics-end-drag)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting dynamic features\n")
(lazy-menu (dynamic format-prog-menu) prog-format-menu prog-format-icons)
(lazy-menu (dynamic fold-menu) insert-fold-menu)
(lazy-menu (dynamic session-menu)
	   supported-sessions-menu insert-session-menu
	   session-menu session-icons session-help-icons help-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting documentation\n")
(lazy-menu (doc help-menu) help-menu)
(lazy-define (doc tmdoc) tmdoc-expand-help tmdoc-expand-help-manual
	     tmdoc-expand-this tmdoc-include)
(lazy-define (doc docgrep) docgrep-in-doc docgrep-in-src docgrep-in-texts)
(lazy-define (doc tmweb) tmweb-convert-dir tmweb-build-from tmweb-build)
(define-secure-symbols tmdoc-include)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting converters\n")
(lazy-format (convert rewrite init-rewrite) texmacs scheme verbatim)
(lazy-format (convert tmml init-tmml) tmml)
(lazy-format (convert latex init-latex) latex)
(lazy-format (convert html init-html) html)
(lazy-format (convert images init-images)
	     postscript pdf xfig xmgrace svg xpm jpeg ppm gif png pnm)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity latex-type)
(lazy-define (convert latex textm) textm-finalize)
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting plugins\n")
(for-each lazy-plugin-initialize (plugin-list))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "Booting fonts\n")
(if (use-ec-fonts?)
    (use-modules (fonts fonts-ec))
    (use-modules (fonts fonts-cm)))
(use-modules (fonts fonts-adobe) (fonts fonts-x) (fonts fonts-math)
	     (fonts fonts-foreign) (fonts fonts-misc))
(if (support-tt-fonts?) (use-modules (fonts fonts-truetype)))
;(display* "time: " (- (texmacs-time) boot-start) "\n")

;(display "------------------------------------------------------\n")
(texmacs-banner)
(delayed (:idle 10000) (delayed-auto-save))
;(display "Initialization done\n")
