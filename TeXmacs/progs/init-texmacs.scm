
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
(retrieve-preferences)

;(display "Booting utilities\n")
(inherit-modules (utils library base) (utils library list)
		 (utils library tree))
(inherit-modules (utils edit generic-edit) (utils misc misc-funcs)
		 (utils misc markup-funcs))
(use-modules (utils cursor cursor-move) (utils edit selections)
	     (utils edit circulate))
(use-modules (utils plugins plugin-cmd))

;(display "Booting TeXmacs fundamental mode\n")
(use-modules (texmacs texmacs tm-server) (texmacs texmacs tm-files)
	     (texmacs texmacs tm-print) (texmacs texmacs tm-view)
	     (texmacs texmacs tm-document))
(use-modules (texmacs edit edit-format)
	     (texmacs edit edit-hybrid))
(re-export safely-kill-window)

(lazy-menu (texmacs menus file-menu)
	   file-menu go-menu
	   new-file-menu load-menu save-menu print-menu close-menu)
(lazy-menu (texmacs menus edit-menu) edit-menu)
(lazy-menu (texmacs menus insert-menu)
	   insert-menu insert-table-menu insert-link-menu insert-switch-menu
	   insert-mathematics-menu insert-session-menu
	   insert-image-menu insert-page-insertion-menu position-float-menu)
(lazy-menu (texmacs menus format-menu) color-menu paragraph-menu format-menu)
(lazy-menu (texmacs menus document-menu)
	   document-menu project-menu document-style-menu global-language-menu)
(lazy-menu (texmacs menus view-menu) view-menu)
(lazy-menu (texmacs menus tools-menu) tools-menu)
(use-modules (texmacs menus preferences-menu) (texmacs menus main-menu))

(use-modules (texmacs keyboard config-kbd))
(lazy-in-mode (texmacs keyboard wildcards-kbd) always?)
(if (like-emacs?) (lazy-in-mode (texmacs keyboard emacs-kbd) always?))
(if (like-windows?) (lazy-in-mode (texmacs keyboard windows-kbd) always?))
(if (like-old?) (lazy-in-mode (texmacs keyboard old-kbd) always?))
(lazy-in-mode (texmacs keyboard latex-kbd) always?)
(lazy-in-mode (texmacs keyboard generic-kbd) always?)

;(display "Booting source mode\n")
(lazy-in-mode (source source-kbd) always?)
(lazy-menu (source source-menu) source-menu texmacs-source-icons
	   source-transformational-menu source-executable-menu)

;(display "Booting text mode\n")
(use-modules (text text-edit))
(lazy-in-mode (text text-kbd) in-text?)
(lazy-menu (text text-menu) size-tag-menu
	   text-menu texmacs-text-icons text-modifier-icons)

;(display "Booting table mode\n")
(use-modules (table table-edit))
(lazy-in-mode (table table-kbd) in-table?)
(lazy-menu (table table-menu) table-menu texmacs-table-icons)

;(display "Booting math mode\n")
(lazy-in-mode (math math-kbd) in-math?)
(lazy-menu (math math-menu)
	   mathematics-menu texmacs-math-icons math-modifier-icons)

;(display "Booting graphics mode\n")
(use-modules (graphics graphics-edit))
(lazy-menu (graphics graphics-menu) graphics-menu texmacs-graphics-icons)

;(display "Booting dynamic features\n")
(use-modules (dynamic fold-edit))
(lazy-menu (dynamic session-menu)
	   prog-modifier-icons session-menu
	   texmacs-session-icons texmacs-session-help-icons texmacs-help-icons)

;(display "Booting documentation\n")
(use-modules (doc help-funcs))
(lazy-menu (doc help-menu) help-menu)
(lazy-define (doc tmdoc) tmdoc-expand-help)
(lazy-define (doc tmdoc) tmdoc-expand-help-manual)
(lazy-define (doc tmdoc) tmdoc-expand-this)
(lazy-define (doc tmdoc) tmdoc-include)
(lazy-define (doc docgrep) docgrep-in-doc)
(lazy-define (doc docgrep) docgrep-in-src)
(lazy-define (doc docgrep) docgrep-in-texts)
(lazy-define (doc tmweb) tmweb-convert-dir)
(lazy-define (doc tmweb) tmweb-build-from)
(lazy-define (doc tmweb) tmweb-build)
(define-secure-symbols tmdoc-include)

;(display "Booting converters\n")
(use-modules (convert rewrite init-rewrite))
(use-modules (convert tmml init-tmml))
(use-modules (convert latex init-latex))
(use-modules (convert html init-html))
(use-modules (convert images init-images))
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity)
(lazy-define (convert latex latex-drd) latex-type)
(lazy-define (convert latex textm) textm-finalize)

;(display "Booting plugins\n")
(if (url-exists? "$TEXMACS_HOME_PATH/system/setup.scm")
    (set! plugin-old-data-table
	  (load-object "$TEXMACS_HOME_PATH/system/setup.scm")))
(for-each plugin-initialize (plugin-list))
(if (!= plugin-old-data-table plugin-data-table)
    (save-object "$TEXMACS_HOME_PATH/system/setup.scm" plugin-data-table))

;(display "Initializing user preferences\n")
(apply-preferences)
(menu-extend page-setup-menu ,@(compute-preferences-menu page-setup-tree))
(menu-extend preferences-menu ,@(compute-preferences-menu preferences-tree))

;(display "Booting fonts\n")
(if (use-ec-fonts?)
    (use-modules (fonts fonts-ec))
    (use-modules (fonts fonts-cm)))
(use-modules (fonts fonts-adobe) (fonts fonts-x) (fonts fonts-math)
	     (fonts fonts-foreign) (fonts fonts-misc))
(if (support-tt-fonts?) (use-modules (fonts fonts-truetype)))
;(display "Initialization done\n")
