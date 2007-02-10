
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-mode.el
;; DESCRIPTION : for editing TeXmacs scheme files with Emacs
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'scheme-mode-hook '(lambda () (texmacs-style)))

(defun texmacs-style ()
  (set-fill-column 79)
  (setq comment-column 40)
  (auto-fill-mode 1)
  (font-lock-add-keywords 'scheme-mode
   (list
    (cons
     (concat "\\<\\("
      (mapconcat 'symbol-name
       '(texmacs-module provide-public define-macro define-public-macro
         tm-define tm-define-macro tm-property request-handler
	 define-table define-preferences define-secure-symbols
         texmacs-modes and-let\\* case-lambda
	 with with-global with-result
         ahash-with and-with for repeat
	 with-innermost with-action with-mutator
	 with-module with-cursor with-server
	 call/cc with-cc values receive map-in-order
         drd-group drd-table drd-dispatcher
         define-grammar drd-rule drd-rules assume menu-bind
         menu-extend menu-dynamic kbd-map kbd-wildcards kbd-commands
         kbd-symbols setup-append-if when link promise while
	 plugin-configure plugin-input-converters use-modules export
	 import-from inherit-modules lazy-menu lazy-keyboard
	 lazy-define lazy-format lazy-input-converter
	 define-format converter with-aux define-group
	 dialogue delayed on-entry on-exit
	 tm-build tm-build-macro tm-build-widget widget-delayed widget-with
	 action alternative alternatives aspect
	 bar block-input button button-alternative button-toggle
	 dense-bar dense-raster
	 form form-cancel form-done form-next form-previous
	 header input internal raster
	 sheet short-bar short-input short-raster suggestions toggle) "\\|")
      "\\)\\>")
     'font-lock-keyword-face)
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name
       '(texmacs-module provide-public
	 define-macro define-public-macro define-table
	 tm-define tm-define-macro tm-property request-handler define-group
	 tm-build tm-build-macro
	 menu-bind menu-extend plugin-configure
	 plugin-input-converters define-format) "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name
       '(converter) "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+ \\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    '("\\<\\(\\sw+%\\)\\>" . font-lock-type-face)))
  (dolist (s '(ahash-with))
    (put s 'scheme-indent-function 3))
  (dolist (s '(with with-global and-with with-innermost receive
	       with-environment with-environment* converter
	       alternatives button-alternative button-toggle sheet))
    (put s 'scheme-indent-function 2))
  (dolist (s '(texmacs-module provide-public with-result
	       and-let* setup-append-if define-group
	       define-macro define-public-macro while for repeat when
	       tm-define tm-define-macro tm-property request-handler
	       tm-build tm-build-macro
	       drd-group drd-table drd-dispatcher menu-bind
	       menu-extend plugin-configure plugin-input-converters
	       with-cc format with-aux with-mutator
	       with-action with-module with-cursor with-server
	       interactive dialogue-user widget-with
	       action aspect block-input button form
	       input internal short-input))
    (put s 'scheme-indent-function 1))
  (dolist (s '(cond call/cc values define-preferences menu-dynamic
	       case-lambda kbd-map kbd-wildcards kbd-commands kbd-symbols
	       define-grammar drd-rule drd-rules assume texmacs-modes
	       delayed dialogue on-entry on-exit widget-delayed
	       bar dense-bar dense-raster header raster
	       sheet short-bar short-raster
	       :use :inherit))
    (put s 'scheme-indent-function 0)))
