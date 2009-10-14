
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-mode.el
;; DESCRIPTION : for editing TeXmacs scheme files with Emacs
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define lists of special keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nullary-keywords
  '(begin cond else call/cc
    values define-preferences menu-dynamic
    case-lambda kbd-map kbd-wildcards kbd-commands kbd-symbols
    define-grammar define-regexp-grammar
    drd-rule drd-rules assume texmacs-modes
    delayed dialogue on-entry on-exit widget-delayed
    association-tile bar concat dense-bar dense-tile document
    header-bar sequence short-bar short-tile tile))

(setq nullary-misc
  '(:use :inherit))

(setq nullary-indent
  (append nullary-keywords nullary-misc))

(setq unary-keywords
  '(let let* lambda
    with-result and-let* setup-append-if
    while for repeat when
    drd-group drd-table drd-dispatcher
    with-cc with-aux
    with-action with-module with-cursor with-server
    dialogue-user widget-with
    aspect block-input button form
    input internal short-input))

(setq unary-definitions
  '(define define-public define-macro define-public-macro
    texmacs-module provide-public define-group
    tm-define tm-define-macro tm-property request-handler
    tm-build tm-build-macro tm-build-widget
    menu-bind menu-extend define-table define-format))

(setq unary-no-highlight
  '(format interactive))

(setq unary-indent
  (append unary-keywords unary-definitions unary-no-highlight))

(setq binary-keywords
  '(with with-global and-with with-innermost receive
    with-environment with-environment* converter
    hidden-input pagelet radio-button toggle-button))

(setq binary-indent
  binary-keywords)

(setq ternary-keywords
  '(ahash-with canvas-input))

(setq ternary-indent
  ternary-keywords)

(setq other-keywords
  '(for if
    define-secure-symbols map-in-order link promise plugin-configure
    plugin-input-converters use-modules export import-from inherit-modules
    lazy-menu lazy-keyboard lazy-define lazy-format lazy-input-converter
    form-cancel form-done form-next form-previous radio suggestions toggle))

(setq highlight-definitions
  unary-definitions)

(setq highlight-keywords
  (append nullary-keywords unary-keywords unary-definitions
	  binary-keywords ternary-keywords other-keywords))

(setq highlight-any
  (append highlight-definitions highlight-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define TeXmacs style
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
      (mapconcat 'symbol-name highlight-keywords "\\|") "\\)\\>")
     'font-lock-keyword-face)
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name highlight-definitions "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name
       '(converter) "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+ \\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    '("\\<\\(\\sw+%\\)\\>" . font-lock-type-face)))
  (dolist (s ternary-indent)
    (put s 'scheme-indent-function 3))
  (dolist (s binary-indent)
    (put s 'scheme-indent-function 2))
  (dolist (s unary-indent)
    (put s 'scheme-indent-function 1))
  (dolist (s nullary-indent)
    (put s 'scheme-indent-function 0)))
