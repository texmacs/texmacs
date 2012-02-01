
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
    values define-preferences menu-dynamic conserve-focus
    case-lambda kbd-map kbd-wildcards kbd-commands kbd-symbols
    define-grammar define-regexp-grammar
    drd-rule drd-rules assume texmacs-modes
    user-delayed delayed dialogue on-entry on-exit widget-delayed
    association-tile bar concat dense-bar dense-tile document
    header-bar sequence short-bar short-tile minibar
    wrap-selection-any wrap-selection-small
    try-modification
    $begin $cond))

(setq nullary-no-highlight
  '(:use :inherit
    $tmdoc $tmdoc-title
    $para $itemize $enumerate
    $description $description-aligned $description-long
    $tm-fragment))

(setq nullary-indent
  (append nullary-keywords nullary-no-highlight))

(setq unary-keywords
  '(let let* lambda
    with-result and-let* setup-append-if
    while for repeat when unless assuming mini tile
    with-focus-after
    drd-group drd-table drd-dispatcher
    with-cc with-aux
    with-action with-module with-cursor with-server
    dialogue-user user-ask
    widget-with aspect block-input button form
    input internal short-input
    $when $let $let* $for))

(setq unary-definitions
  '(define define-public define-macro define-public-macro
    texmacs-module provide-public define-group
    tm-define tm-define-macro tm-property request-handler
    tm-menu define-menu tm-generate
    tm-build tm-build-macro tm-build-widget
    menu-bind menu-extend define-table
    define-format define-language))

(setq unary-no-highlight
  '(format interactive
    $describe-item $link $tmdoc-link
    $folded-documentation $unfolded-documentation $explain))

(setq unary-indent
  (append unary-keywords unary-definitions unary-no-highlight))

(setq binary-keywords
  '(with with-define with-global and-with with-innermost receive
    with-environment with-environment* converter
    user-confirm user-url
    hidden-input pagelet radio-button toggle-button
    $with))

(setq binary-indent
  binary-keywords)

(setq ternary-keywords
  '(ahash-with canvas-input))

(setq ternary-indent
  ternary-keywords)

(setq other-keywords
  '(for if inherit
    define-secure-symbols map-in-order link promise plugin-configure
    plugin-input-converters use-modules export import-from inherit-modules
    lazy-menu lazy-keyboard lazy-define lazy-initialize
    lazy-format lazy-language lazy-input-converter
    form-cancel form-done form-next form-previous radio suggestions toggle
    $if))

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
