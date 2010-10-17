
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-kbd.scm
;; DESCRIPTION : general keyboard shortcuts for all modes
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-kbd)
  (:use (texmacs keyboard prefix-kbd)
	(utils edit variants)
	(utils edit auto-close)
	(utils library cursor)
	(generic generic-edit)
	(source source-edit)
	(texmacs texmacs tm-files)
	(texmacs texmacs tm-print)
	(doc help-funcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General shortcuts for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("F1" (interactive docgrep-in-doc))
  ("S-F1" (noop)) ;; FIXME: should be: 'What is This?'
  ("C-F1" (load-help-buffer "about/welcome/welcome"))
  ("C-F9" (toggle-full-screen-mode))

  ("<" "<less>")
  (">" "<gtr>")
  ("(" (make-bracket-open "(" ")"))
  (")" (make-bracket-close ")" "("))
  ("[" (make-bracket-open "[" "]"))
  ("]" (make-bracket-close "]" "["))
  ("{" (make-bracket-open "{" "}"))
  ("}" (make-bracket-close "}" "{"))
  ("\\" (if (or (inside? 'hybrid) (in-prog?)) (insert "\\") (make-hybrid)))

  ("backspace" (kbd-remove #f))
  ("delete" (kbd-remove #t))
  ("left" (kbd-left))
  ("right" (kbd-right))
  ("up" (kbd-up))
  ("down" (kbd-down))
  ("home" (kbd-start-line))
  ("end" (kbd-end-line))
  ("pageup" (kbd-page-up))
  ("pagedown" (kbd-page-down))
  ("S-backspace" (kbd-remove #f))
  ("S-delete" (kbd-remove #t))
  ("S-left" (kbd-select kbd-left))
  ("S-right" (kbd-select kbd-right))
  ("S-up" (kbd-select kbd-up))
  ("S-down" (kbd-select kbd-down))
  ("S-home" (kbd-select kbd-start-line))
  ("S-end" (kbd-select kbd-end-line))
  ("S-pageup" (kbd-select kbd-page-up))
  ("S-pagedown" (kbd-select kbd-page-down))

  ("C-<" (cursor-history-backward))
  ("C->" (cursor-history-forward))
  ("space" (kbd-space))
  ("space var" (make 'nbsp))
  ("space var var" (make-space "0.2spc"))
  ("space var var var" (make-space "1em"))
  ("return" (kbd-return))
  ("tab" (kbd-tab))
  ("S-space" (kbd-shift-space))
  ("S-return" (kbd-shift-return))
  ("C-space" (kbd-select-enlarge))
  ("C-tab" (variant-circulate #t))
  ("C-S-tab" (variant-circulate #f))
  ("C-*" (hidden-variant))
  ("C-%" (toggle-variant))
  ("A-tab" (make-htab "5mm"))
  ("A-*" (toggle-number))
  ("A-space" (make-space "0.2spc"))
  ("A-S-space" (make-space "-0.2spc"))
  ("M-space" (make-space "0.2spc"))
  ("M-S-space" (make-space "-0.2spc"))
  ("M-tab" (make-htab "5mm"))
  ("escape tab" (noop) (make-htab "5mm"))

  ("structured:cmd backspace" (remove-structure-upwards))
  ("structured:cmd delete" (remove-structure-upwards))
  ("structured:cmd left" (traverse-left))
  ("structured:cmd right" (traverse-right))
  ("structured:cmd up" (traverse-up))
  ("structured:cmd down" (traverse-down))
  ("structured:cmd home" (traverse-first))
  ("structured:cmd end" (traverse-last))
  ("structured:cmd pageup" (traverse-previous))
  ("structured:cmd pagedown" (traverse-next))
  ("structured:cmd section" (traverse-previous-section-title))
  ("structured:cmd S-left" (kbd-select traverse-left))
  ("structured:cmd S-right" (kbd-select traverse-right))
  ("structured:cmd S-up" (kbd-select traverse-up))
  ("structured:cmd S-down" (kbd-select traverse-down))
  ("structured:cmd S-home" (kbd-select traverse-first))
  ("structured:cmd S-end" (kbd-select traverse-last))
  ("structured:cmd S-pageup" (kbd-select traverse-previous))
  ("structured:cmd S-pagedown" (kbd-select traverse-next))

  ("structured:move backspace" (structured-exit-left))
  ("structured:move delete" (structured-exit-right))
  ("structured:move left" (structured-left))
  ("structured:move right" (structured-right))
  ("structured:move up" (structured-up))
  ("structured:move down" (structured-down))
  ("structured:move home" (structured-first))
  ("structured:move end" (structured-last))
  ("structured:move pageup" (structured-top))
  ("structured:move pagedown" (structured-bottom))
  ("structured:move S-left" (kbd-select structured-left))
  ("structured:move S-right" (kbd-select structured-right))
  ("structured:move S-up" (kbd-select structured-up))
  ("structured:move S-down" (kbd-select structured-down))
  ("structured:move S-home" (kbd-select structured-first))
  ("structured:move S-end" (kbd-select structured-last))
  ("structured:move S-pageup" (kbd-select structured-top))
  ("structured:move S-pagedown" (kbd-select structured-bottom))

  ("structured:insert backspace" (structured-remove #f))
  ("structured:insert delete" (structured-remove #t))
  ("structured:insert left" (structured-insert #f))
  ("structured:insert right" (structured-insert #t))
  ("structured:insert up" (structured-insert-up))
  ("structured:insert down" (structured-insert-down))
  ("structured:insert home" (structured-insert-start))
  ("structured:insert end" (structured-insert-end))
  ("structured:insert pageup" (structured-insert-top))
  ("structured:insert pagedown" (structured-insert-bottom))

  ("structured:geometry backspace" (positioning-default))
  ("structured:geometry delete" (positioning-default))
  ("structured:geometry left" (positioning-left))
  ("structured:geometry right" (positioning-right))
  ("structured:geometry up" (positioning-up))
  ("structured:geometry down" (positioning-down))
  ("structured:geometry home" (positioning-start))
  ("structured:geometry end" (positioning-end))
  ("structured:geometry pageup" (positioning-top))
  ("structured:geometry pagedown" (positioning-bottom))

  ("altcmd \\" (make-hybrid))
  ("altcmd a" (make-tree))
  ("altcmd R" (make-rigid))
  ("altcmd :" (make 'line-break))
  ("altcmd ;" (make 'new-line))
  ("altcmd return" (make 'next-line))
  ("altcmd /" (make 'no-break))
  ("altcmd !" (make-label))
  ("altcmd ?" (make 'reference))
  ("altcmd C-?" (make 'pageref))

  ("accent:hat" "^")
  ("accent:deadhat" "^")
  ("accent:tilde" "~")
  ("accent:acute" "'")
  ("accent:grave" "`")

  ("symbol \\" "\\")
  ("symbol \"" "\"")
  ("symbol $" "$")
  ("symbol &" "&")
  ("symbol #" "#")
  ("symbol §" "§")
  ("symbol %" "%")
  ("symbol _" "_")
  ("symbol ^" "^")
  ("symbol \"" "\"")
  ("symbol (" "(")
  ("symbol )" ")")
  ("symbol [" "[")
  ("symbol ]" "]")
  ("symbol {" "{")
  ("symbol }" "}")
  ("symbol |" "|")

  ("undo" (noop) (undo 0))
  ("redo" (noop) (redo 0))
  ("cancel" (noop) (clipboard-clear "primary"))
  ("cut" (noop) (clipboard-cut "primary"))
  ("paste" (noop) (clipboard-paste "primary"))
  ("copy" (noop) (clipboard-copy "primary"))
  ("find" (noop) (search-start #t))

  ("copyto 1" (noop) (clipboard-copy "primary"))
  ("copyto 2" (clipboard-copy "secondary"))
  ("copyto 3" (clipboard-copy "ternary"))
  ("copyto s" (clipboard-copy "search"))
  ("copyto r" (clipboard-copy "replace"))
  ("copyto *" (interactive clipboard-copy))
  ("cutto 1" (noop) (clipboard-cut "primary"))
  ("cutto 2" (clipboard-cut "secondary"))
  ("cutto 3" (clipboard-cut "ternary"))
  ("cutto s" (clipboard-cut "search"))
  ("cutto r" (clipboard-cut "replace"))
  ("cutto *" (interactive clipboard-cut))
  ("pastefrom 1" (noop) (clipboard-paste "primary"))
  ("pastefrom 2" (clipboard-paste "secondary"))
  ("pastefrom 3" (clipboard-paste "ternary"))
  ("pastefrom s" (clipboard-paste "search"))
  ("pastefrom r" (clipboard-paste "replace"))
  ("pastefrom *" (interactive clipboard-paste))

  ("table N t" (make 'tabular))
  ("table N T" (make 'tabular*))
  ("table N b" (make 'block))
  ("table N B" (make 'block*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile emacs)

  ;; standard Emacs shortcuts
  ("emacs a" (kbd-start-line))
  ("emacs b" (kbd-left))
  ("emacs d" (remove-text #t))
  ("emacs e" (kbd-end-line))
  ("emacs f" (kbd-right))
  ("emacs g" (selection-cancel))
  ("emacs j" (insert-return))
  ("emacs k" (kill-paragraph))
  ("emacs l" (recenter-window))
  ("emacs m" (insert-return))
  ("emacs n" (kbd-down))
  ("emacs o" (open-line))
  ("emacs p" (kbd-up))
  ("emacs q" (make 'symbol))
  ("emacs r" (search-start #f))
  ("emacs s" (search-start #t))
  ("emacs t" (transpose-chars))
  ("emacs v" (kbd-page-down))
  ("emacs w" (clipboard-cut "primary"))
  ("emacs y" (clipboard-paste "primary"))
  ("emacs insert" (clipboard-copy "primary"))
  ("emacs \\" (toggle-input-method))
  ("emacs ]" (abort-recursive-edit))
  ("emacs _" (undo 0))
  ("emacs /" (undo 0))

  ("emacs:meta v" (kbd-page-up))
  ("emacs:meta w" (clipboard-copy "primary"))
  ("emacs:meta x" (interactive exec-interactive-command))
  ("emacs:meta X" (interactive footer-eval))
  ("emacs:meta <" (go-start))
  ("emacs:meta >" (go-end))
  ("emacs:meta $" (spell-start))

  ("emacs:prefix b" (interactive go-to-buffer))
  ("emacs:prefix k" (safely-kill-buffer))
  ("emacs:prefix C-c" (safely-quit-TeXmacs))
  ("emacs:prefix C-f" (interactive load-buffer))
  ("emacs:prefix C-s" (save-buffer))
  ("emacs:prefix C-w" (interactive save-buffer))

  ;; extra Emacs-ish shortcuts
  ("emacs =" (interactive replace-start-forward))

  ("emacs:meta g" (clipboard-clear "primary"))
  ("emacs:meta [" (undo 0))
  ("emacs:meta ]" (redo 0))

  ;; further shortcuts for the Emacs mode
  ("F2" (open-buffer))
  ("S-F2" (choose-file load-in-new-window "Load file" ""))
  ("C-F2" (revert-buffer))
  ("M-F2" (new-buffer))
  ("M-S-F2" (open-window))
  ("M-C-F2" (clone-window))
  ("F3" (save-buffer))
  ("S-F3" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ("F4" (preview-with-ghostview))
  ("S-F4" (print-buffer))
  ("C-F4" (interactive print-to-file))
  ("M-F4" (interactive print-pages))
  ("M-S-F4" (interactive print-pages-to-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard cross-platform keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile std)

  ;; standard cross-platform shortcuts
  ("std c" (clipboard-copy "primary"))
  ("std f" (search-start #t))
  ("std n" (new-buffer))
  ("std o" (open-buffer))
  ("std p" (preview-with-ghostview))
  ("std q" (safely-quit-TeXmacs))
  ("std r" (interactive replace-start-forward))
  ("std s" (save-buffer))
  ("std S" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ("std v" (clipboard-paste "primary"))
  ("std w" (safely-kill-buffer))
  ("std x" (clipboard-cut "primary"))
  ("std z" (undo 0))
  ("std Z" (redo 0))

  ;; extra cross-platform shortcuts
  ("std F" (search-start #f))

  ("altcmd x" (interactive footer-eval))
  ("altcmd <" (go-start))
  ("altcmd >" (go-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnome keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile gnome)

  ;; extra gnome-ish shortcuts
  ("std g" (selection-cancel))
  ("std k" (kill-paragraph))
  ("std F" (search-start #f))

  ("altcmd g" (clipboard-clear "primary"))
  ("altcmd q" (make 'symbol))
  ("altcmd $" (spell-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnome and windows keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile kde)

  ;; extra kde-ish shortcuts
  ("std g" (selection-cancel))
  ("std k" (kill-paragraph))
  ("std F" (search-start #f))

  ("altcmd g" (clipboard-clear "primary"))
  ("altcmd q" (make 'symbol))
  ("altcmd $" (spell-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile macos)

  ;; standard Mac OS keyboard shortcuts
  ("std ;" (spell-start))
  ("std ?" (interactive docgrep-in-doc))

  ;; extra MacOS-like shortcuts
  ("C-g" (selection-cancel))
  ("C-k" (kill-paragraph))
  ("C-q" (make 'symbol))

  ("altcmd X" (interactive footer-eval)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile windows)

  ;; extra Window-ish shortcuts
  ("std g" (selection-cancel))
  ("std k" (kill-paragraph))
  ("std F" (search-start #f))

  ("altcmd g" (clipboard-clear "primary"))
  ("altcmd q" (make 'symbol))
  ("altcmd $" (spell-start)))
