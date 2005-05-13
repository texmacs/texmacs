
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
       '(texmacs-module define-macro define-public-macro
         tm-define tm-define-macro tm-property
	 define-table define-preferences define-secure-symbols
         texmacs-modes and-let* case-lambda with values receive map-in-order
         drd-group drd-table drd-dispatcher
         define-grammar drd-rule drd-rules assume menu-bind
         menu-extend menu-dynamic kbd-map kbd-wildcards kbd-commands
         kbd-symbols setup-append-if when link promise
	 plugin-configure plugin-input-converters use-modules export
	 import-from inherit-modules lazy-menu lazy-in-mode
	 lazy-define lazy-format lazy-input-converter delayed
	 define-format converter
	 with-aux) "\\|")
      "\\)\\>")
     'font-lock-keyword-face)
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name
       '(texmacs-module define-macro define-public-macro define-table
	 tm-define tm-define-macro tm-property
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
  (dolist (s '(with receive with-environment with-environment* converter))
    (put s 'scheme-indent-function 2))
  (dolist (s '(texmacs-module and-let* setup-append-if
	       define-macro define-public-macro
	       tm-define tm-define-macro tm-property
	       drd-group drd-table drd-dispatcher menu-bind
	       menu-extend plugin-configure plugin-input-converters
	       format with-aux))
    (put s 'scheme-indent-function 1))
  (dolist (s '(values define-preferences menu-dynamic case-lambda
	       kbd-map kbd-wildcards kbd-commands kbd-symbols
	       define-grammar drd-rule drd-rules assume texmacs-modes
	       delayed
	       :use :inherit))
    (put s 'scheme-indent-function 0)))
