
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
(retrieve-preferences)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting utilities\n")
(inherit-modules (utils library base) (utils library list)
		 (utils library tree))
(inherit-modules (utils misc misc-funcs) (utils misc markup-funcs))
(use-modules (utils edit selections) (utils edit variants))
(use-modules (utils plugins plugin-cmd))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting main TeXmacs functionality\n")
(lazy-menu (texmacs menus file-menu)
	   file-menu go-menu
	   new-file-menu load-menu save-menu print-menu close-menu)
(lazy-menu (texmacs menus edit-menu) edit-menu)
(lazy-menu (texmacs menus view-menu) view-menu)
(lazy-menu (texmacs menus tools-menu) tools-menu)
(use-modules (texmacs menus preferences-menu) (texmacs menus main-menu))
(use-modules (texmacs keyboard config-kbd))
(lazy-in-mode (texmacs keyboard wildcards-kbd) always?)
(if (like-emacs?) (lazy-in-mode (texmacs keyboard emacs-kbd) always?))
(if (like-windows?) (lazy-in-mode (texmacs keyboard windows-kbd) always?))
(lazy-in-mode (texmacs keyboard latex-kbd) always?)
(lazy-in-mode (texmacs keyboard texmacs-kbd) always?)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting generic mode\n")
(lazy-in-mode (generic generic-kbd) always?)
(lazy-menu (generic format-menu) format-menu font-size-menu color-menu)
(lazy-menu (generic document-menu)
	   document-menu project-menu document-style-menu global-language-menu)
(lazy-menu (generic insert-menu)
	   insert-menu insert-link-menu insert-image-menu
	   insert-page-insertion-menu position-float-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting source mode\n")
(lazy-in-mode (source source-kbd) always?)
(lazy-menu (source source-menu) source-menu source-icons
	   source-transformational-menu source-executable-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting text mode\n")
(lazy-in-mode (text text-kbd) in-text?)
(lazy-in-mode (text format-text-kbd) in-text?)
(lazy-in-mode (text std-text-kbd) in-std-text?)
(lazy-menu (text format-text-menu) text-format-menu text-format-icons)
(lazy-menu (text text-menu) text-menu text-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting table mode\n")
(lazy-in-mode (table table-kbd) in-table?)
(lazy-menu (table table-menu) table-menu table-icons insert-table-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting math mode\n")
(lazy-in-mode (math math-kbd) in-math?)
(lazy-menu (math format-math-menu) math-format-menu math-format-icons)
(lazy-menu (math math-menu) math-menu math-icons insert-math-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting graphics mode\n")
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting dynamic features\n")
(lazy-menu (dynamic format-prog-menu) prog-format-menu prog-format-icons)
(lazy-menu (dynamic fold-menu) insert-fold-menu)
(lazy-menu (dynamic session-menu)
	   supported-sessions-menu insert-session-menu
	   session-menu session-icons session-help-icons help-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting documentation\n")
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
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(define-macro (delayed-use-modules . l)
;  `(exec-delayed-cmd (object->command (lambda () (use-modules ,@l)))))
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
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting plugins\n")
(if (url-exists? "$TEXMACS_HOME_PATH/system/setup.scm")
    (set! plugin-old-data-table
	  (load-object "$TEXMACS_HOME_PATH/system/setup.scm")))
(define (delayed-plugin-initialize which)
  (exec-delayed-cmd (object->command (lambda () (plugin-initialize which)))))
(for-each delayed-plugin-initialize (plugin-list))
(exec-delayed-cmd
 (object->command
  (lambda ()
    (if (!= plugin-old-data-table plugin-data-table)
	(save-object "$TEXMACS_HOME_PATH/system/setup.scm"
		     plugin-data-table)))))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Initializing user preferences\n")
(apply-preferences)
(menu-extend page-setup-menu ,@(compute-preferences-menu page-setup-tree))
(menu-extend preferences-menu ,@(compute-preferences-menu preferences-tree))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Booting fonts\n")
(if (use-ec-fonts?)
    (use-modules (fonts fonts-ec))
    (use-modules (fonts fonts-cm)))
(use-modules (fonts fonts-adobe) (fonts fonts-x) (fonts fonts-math)
	     (fonts fonts-foreign) (fonts fonts-misc))
(if (support-tt-fonts?) (use-modules (fonts fonts-truetype)))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(define boot-start (texmacs-time))

;(display "Initialization done\n")
