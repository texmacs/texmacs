
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

;(display "Booting TeXmacs kernel functionality\n")
(load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot.scm"))
(inherit-modules (kernel boot compat) (kernel boot abbrevs)
		 (kernel boot debug) (kernel boot srfi)
		 (kernel boot ahash-table) (kernel boot prologue))
(inherit-modules (kernel regexp regexp-match))
(inherit-modules (kernel drd drd-rules) (kernel drd drd-query)
		 (kernel drd drd-data))
(inherit-modules (kernel texmacs tm-define) (kernel texmacs tm-preferences)
		 (kernel texmacs tm-plugins) (kernel texmacs tm-modes)
		 (kernel texmacs tm-secure))
(inherit-modules (kernel library base) (kernel library list))
(inherit-modules (kernel gui menu-define) (kernel gui menu-widget)
		 (kernel gui kbd-define))
(inherit-modules (kernel tools tm-edit) (kernel tools tm-misc)
		 (kernel tools tm-convert))
(retrieve-preferences)

;(display "Booting TeXmacs primitives\n")
(use-modules (texmacs texmacs tm-server) (texmacs texmacs tm-files)
	     (texmacs texmacs tm-print) (texmacs texmacs tm-view)
	     (texmacs texmacs tm-document) (texmacs texmacs tm-help))
(use-modules (texmacs tools tm-cursor) (texmacs tools tm-select)
	     (texmacs tools tm-circulate))
(use-modules (texmacs edit edit-preamble) (texmacs edit edit-format)
	     (texmacs edit edit-text) (texmacs edit edit-table)
	     (texmacs edit edit-fold) (texmacs edit edit-misc)
	     (texmacs edit edit-hybrid))
(re-export safely-kill-window)

;(display "Booting menus\n")
(lazy-menu (menus menu-file)
	   file-menu go-menu
	   new-file-menu load-menu save-menu print-menu close-menu)
(lazy-menu (menus menu-edit) edit-menu)
(lazy-menu (menus menu-insert)
	   insert-menu insert-table-menu insert-link-menu insert-switch-menu
	   insert-image-menu insert-page-insertion-menu position-float-menu)
(lazy-menu (menus menu-text)
	   text-menu texmacs-text-icons text-modifier-icons)
(lazy-menu (menus menu-mathematics)
	   mathematics-menu texmacs-math-icons math-modifier-icons)
(lazy-menu (menus menu-prog)
	   prog-modifier-icons session-menu
	   texmacs-session-icons texmacs-session-help-icons texmacs-help-icons)
(lazy-menu (menus menu-table) table-menu texmacs-table-icons)
(lazy-menu (menus menu-format) color-menu paragraph-menu format-menu)
(lazy-menu (menus menu-document)
	   document-menu project-menu document-style-menu global-language-menu)
(lazy-menu (menus menu-view) view-menu)
(lazy-menu (menus menu-tools) tools-menu)
(lazy-menu (menus menu-help) help-menu)
(use-modules (menus menu-preferences) (menus menu-main))

;(display "Booting keyboard\n")
;Note: lazyness implies preferences initialization to take place first
(use-modules (keyboard kbd-config))
(lazy-in-mode (keyboard kbd-wildcards) always?)
(if (like-emacs?) (lazy-in-mode (keyboard kbd-emacs) always?))
(if (like-old?) (lazy-in-mode (keyboard kbd-old) always?))
(lazy-in-mode (keyboard kbd-latex) always?)
(lazy-in-mode (keyboard kbd-general) always?)
(lazy-in-mode (keyboard kbd-preamble) always?)
(lazy-in-mode (keyboard kbd-text) in-text?)
(lazy-in-mode (keyboard kbd-math) in-math?)
(lazy-in-mode (keyboard kbd-table) in-table?)

;(display "Booting fonts\n")
(if (use-ec-fonts?)
    (use-modules (fonts fonts-ec))
    (use-modules (fonts fonts-cm)))
(use-modules (fonts fonts-adobe) (fonts fonts-x) (fonts fonts-math)
	     (fonts fonts-foreign) (fonts fonts-misc))
(if (use-tt-fonts?) (use-modules (fonts fonts-truetype)))

;(display "Booting converters\n")
(use-modules (convert rewrite init-rewrite))
(use-modules (convert tmml init-tmml))
(use-modules (convert latex init-latex))
(use-modules (convert html init-html))
(use-modules (convert images init-images))
(use-modules (convert rewrite tmtm-eqns))
(lazy-define (drd latex latex-drd) latex-arity)
(lazy-define (drd latex latex-drd) latex-type)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert doc tmdoc) tmdoc-expand-help)
(lazy-define (convert doc tmdoc) tmdoc-expand-help-manual)
(lazy-define (convert doc tmdoc) tmdoc-expand-this)
(lazy-define (convert doc tmdoc) tmdoc-include)
(lazy-define (convert doc docgrep) docgrep-in-doc)
(lazy-define (convert doc docgrep) docgrep-in-src)
(lazy-define (convert doc docgrep) docgrep-in-texts)
(lazy-define (convert doc tmweb) tmweb-convert-dir)
(lazy-define (convert doc tmweb) tmweb-build-from)
(lazy-define (convert doc tmweb) tmweb-build)
(secure-symbols tmdoc-include)

;(display "Booting plugins\n")
(lazy-define (texmacs plugin plugin-cmd) verbatim-serialize)
(lazy-define (texmacs plugin plugin-cmd) plugin-serialize)
(lazy-define (texmacs plugin plugin-cmd) format-command)
(if (url-exists? "$TEXMACS_HOME_PATH/system/setup.scm")
    (set! plugin-old-data-table
	  (load-object "$TEXMACS_HOME_PATH/system/setup.scm")))
(for-each plugin-initialize (plugin-list))
(if (not (== plugin-old-data-table plugin-data-table))
    (save-object "$TEXMACS_HOME_PATH/system/setup.scm" plugin-data-table))

;(display "Initializing user preferences\n")
(apply-preferences)
(menu-extend page-setup-menu ,@(compute-preferences-menu page-setup-tree))
(menu-extend preferences-menu ,@(compute-preferences-menu preferences-tree))
;(display "Initialization done\n")
