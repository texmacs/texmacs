
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
  '(begin cond else
    values define-preferences menu-dynamic conserve-focus attach-macro
    case-lambda kbd-map kbd-wildcards kbd-commands kbd-symbols
    define-grammar define-regexp-grammar
    drd-rule logic-rules assume texmacs-modes
    user-delayed delayed on-entry on-exit widget-delayed
    with-wallet
    association-tile bar concat dense-bar dense-tile document
    header-bar sequence short-bar short-tile minibar
    wrap-selection-any wrap-selection-small
    try-modification try-correct
    tabs icon-tabs padded centered aligned bottom-buttons scrollable
    hlist vlist hsplit vsplit explicit-buttons horizontal vertical
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
    plugin-configure
    define-preference-names
    with-focus-after
    logic-group logic-table logic-dispatcher
    with-aux with-action with-module
    with-cursor with-buffer with-author
    with-server with-database with-database*
    with-time with-time-stamp with-limit
    with-encoding with-indexing
    with-user with-extra-fields
    with-remote-context with-identifier-context
    user-ask
    tab icon-tab form item meti refreshable
    $when $let $let* $for $refreshable
    tmfs-load-handler tmfs-save-handler
    tmfs-autosave-handler tmfs-remove-handler tmfs-wrap-handler
    tmfs-date-handler tmfs-title-handler tmfs-permission-handler
    tmfs-master-handler tmfs-format-handler
    push-focus pull-focus))

(setq unary-definitions
  '(define define-public define-macro define-public-macro
    texmacs-module provide-public define-group
    tm-define tm-define-macro lazy-body-macro tm-property request-handler
    tm-menu define-menu tm-widget define-widget tm-generate
    tm-build tm-build-macro tm-build-widget
    menu-bind define-table extend-table smart-table
    tm-service tm-call-back
    define-format define-language define-graphics))

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
    resize
    $with))

(setq binary-no-highlight
  '(client-remote-eval server-remote-eval))

(setq binary-indent
  (append binary-keywords binary-no-highlight))

(setq ternary-keywords
  '(ahash-with canvas-input
    with-remote-get-attributes
    with-remote-get-entry with-remote-create-entry
    with-remote-search with-remote-search-user
    with-remote-get-user-pseudo with-remote-get-user-name
    with-remote-identifier))

(setq ternary-indent
  ternary-keywords)

(setq quaternary-keywords
  '(with-remote-get-field))

(setq quaternary-indent
  quaternary-keywords)

(setq other-keywords
  '(for if inherit former
    define-secure-symbols map-in-order link promise
    plugin-input-converters use-modules export import-from inherit-modules
    lazy-menu lazy-keyboard lazy-define lazy-define-macro lazy-initialize
    lazy-format lazy-language lazy-input-converter lazy-tmfs-handler
    $if))

(setq highlight-definitions
  unary-definitions)

(setq highlight-keywords
  (append nullary-keywords unary-keywords unary-definitions
	  binary-keywords ternary-keywords quaternary-keywords
          other-keywords))

(setq highlight-any
  (append highlight-definitions highlight-keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define TeXmacs style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'scheme-mode-hook '(lambda () (texmacs-style)))

(defun symbol-var-name (name)
  (let ((r (symbol-name name)))
    (let ((n (length r))
          (l (max (- (length r) 1) 0)))
      (let ((head (substring r 0 l))
            (tail (substring r l)))
        (if (string= tail "*") (concat head "\\*") r)))))

(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (* 7 number))

(defun texmacs-style ()
  (set-fill-column 79)
  (setq comment-column 40)
  (auto-fill-mode 1)
  (font-lock-add-keywords 'scheme-mode
   (list
    (cons
     (concat "\\<\\("
      (mapconcat 'symbol-var-name highlight-keywords "\\|") "\\)\\>")
     'font-lock-keyword-face)
    (cons
     (concat "(\\("
      (mapconcat 'symbol-var-name highlight-definitions "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    (cons
     (concat "(\\("
      (mapconcat 'symbol-var-name
       '(converter) "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+ \\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    '("\\<\\(\\sw+%\\)\\>" . font-lock-type-face)))
  (dolist (s quaternary-indent)
    (put s 'scheme-indent-function 4))
  (dolist (s ternary-indent)
    (put s 'scheme-indent-function 3))
  (dolist (s binary-indent)
    (put s 'scheme-indent-function 2))
  (dolist (s unary-indent)
    (put s 'scheme-indent-function 1))
  (dolist (s nullary-indent)
    (put s 'scheme-indent-function 0)))
