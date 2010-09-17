
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : emacs-kbd.scm
;; DESCRIPTION : keyboard shortcuts for emacs look and feel
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard emacs-kbd)
  (:use
    (utils library cursor)
    (utils edit selections)
    (texmacs texmacs tm-server)
    (texmacs texmacs tm-files)
    (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wildcards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-variant-keys "tab" "S-tab")

(kbd-wildcards pre
  ("emacs" "C-" #t)
  ("emacs:contextual" "emacs c")
  ("emacs:prefix" "emacs x")

  ("cmd" "M-")
  ("font" "M-A-")
  ("text" "A-")
  ("math" "A-")
  ("prog" "A-")
  ("special" "H-")
  ("symbol" "S-F5" #t)
  ("executable" "cmd e")
  ("inactive" "cmd i")
  ("link" "cmd l")
  ("version" "cmd #")
  ("table" "cmd t")
  ("script" "cmd *")
  ("text:symbol" "S-F5" #t)
  ("var" "tab" #t)
  ("unvar" "S-tab" #t)
  ("copyto" "M-W")
  ("cutto" "emacs W")
  ("pastefrom" "emacs Y")
  
  ("accent:tilde" "A-~")
  ("accent:hat" "A-^")
  ("accent:umlaut" "A-\"")
  ("accent:acute" "A-'")
  ("accent:grave" "A-`")
  ("accent:cedilla" "A-C")
  ("accent:breve" "A-U")
  ("accent:check" "A-V")
  ("accent:doubleacute" "A-H")
  ("accent:abovering" "A-O")
  ("accent:abovedot" "A-.")
  ("accent:ogonek" "A-G")

  ("math:greek" "F5" #t)
  ("math:bold" "F6" #t)
  ("math:bold:greek" "math:bold math:greek" #t)
  ("math:bold:cal" "math:bold math:cal" #t)
  ("math:cal" "F7" #t)
  ("math:frak" "F8" #t)
  ("math:bbb" "S-F6" #t)
  ("math:symbol" "S-F5" #t)
  ("math:symbol:circled" "math:symbol @" #t)
  ("math:symbol:limits" "math:symbol L" #t)
  ("math:over" "math o" #t)
  ("math:under" "math u" #t)
  ("math:large" "math" #t)
  ("math:left" "math l" #t)
  ("math:middle" "math m" #t)
  ("math:right" "math r" #t))

(kbd-wildcards
  ("escape" "M-" #t)
  ("escape escape" "A-" #t)
  ("escape escape escape" "H-" #t)
  ("escape escape escape escape" "noop" #t)
  ("S-escape" "A-" #t)
  ("C-escape" "H-" #t)

  ("tilde" "A-~")
  ("hat" "accent:deadhat") ;; needed for dead ^ in math mode
  ("umlaut" "A-\"")
  ("acute" "A-'")
  ("grave" "A-`")
  ("cedilla" "A-C")
  ("breve" "A-U")
  ("check" "A-V")
  ("doubleacute" "A-H")
  ("abovering" "A-O")
  ("abovedot" "A-.")
  ("ogonek" "A-G")

  ;; for convenience
  ("M-~" "A-~")
  ("M-^" "A-^")
  ("M-\"" "A-\"")
  ("M-'" "A-'")
  ("M-`" "A-`")
  ("M-C" "A-C")
  ("M-U" "A-U")
  ("M-V" "A-V")
  ("M-H" "A-H")
  ("M-O" "A-O")
  ("M-." "A-.")
  ("M-G" "A-G"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Explain prefixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("emacs" "" "Emacs command")
  ("emacs:contextual" "" "Emacs mode specific prefix command")
  ("emacs:prefix" "" "Emacs prefix command")

  ("cmd" "" "Execute a TeXmacs command")
  ("noop" (set-message "" ""))
  ("symbol" "" "Insert a TeXmacs symbol")
  ("executable" "" "Insert executable markup")
  ("inactive" "" "Insert inactive markup")
  ("text" "" "Insert structural markup")
  ("text:symbol" "" "Insert a TeXmacs symbol")
  ("special" "" "Special command")
  ("script" "" "Evaluate function or insert evaluation tag")
  ("copyto" "" "Copy to (1, 2, 3, *:other)")
  ("cutto" "" "Cut to (1, 2, 3, *:other)")
  ("pastefrom" "" "Paste from (1, 2, 3, *:other)")

  ("table" "" "Table command")
  ("table N" "" "New table (t: tabular, b: block)")
  ("table H" "" "Horizontal table alignment (l: left, c: center, r: right)")
  ("table V" "" "Vertical table alignment: (b: bottom, c: center, t: top)")
  ("table B" "" "Modify table border (options: =, l, r, b, t)")
  ("table P" "" "Modify table padding (options: =, l, r, b, t)")
  ("table m" "" "Set cell mode (c: cell, h: row, v: column, t: table)")
  ("table h" "" "Horizontal cell alignment (l: left, c: center, r: right)")
  ("table v" "" "Vertical cell alignment: (b: bottom, c: center, t: top)")
  ("table b" "" "Modify cell border (options: =, l, r, b, t)")
  ("table p" "" "Modify cell padding (options: =, l, r, b, t)"))

(kbd-map
  (:mode in-math?)
  ("math" "" "Insert mathematical markup")
  ("math:greek" "" "Insert a Greek character")
  ("math:bold" "" "Insert a bold character")
  ("math:bold:greek" "" "Insert a bold Greek character")
  ("math:cal" "" "Insert a calligraphic character")
  ("math:bold:cal" "" "Insert a bold calligraphic character")
  ("math:frak" "" "Insert a fraktur character")
  ("math:bbb" "" "Insert a blackboard bold character")
  ("math:over" "" "Insert a wide symbol above")
  ("math:under" "" "Insert a wide symbol below")
  ("math:left" "" "Insert a large left delimiter or left subscript")
  ("math:middle" "" "Insert a large separator")
  ("math:right" "" "Insert a large right delimiter")
  ("math:symbol" "" "Insert a mathematical symbol")
  ("math:symbol:circled" "" "Insert a big circled operator")
  ("math:symbol:limits" "" "Insert a mathematical symbol with limits"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Emacs commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
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
  ("emacs space" (kbd-select-enlarge))
  ("emacs tab" (kbd-select-environment))

  ("M-v" (kbd-page-up))
  ("M-w" (clipboard-copy "primary"))
  ("M-x" (interactive exec-interactive-command))
  ("M-X" (interactive footer-eval))
  ("M-<" (go-start))
  ("M->" (go-end))
  ("M-$" (spell-start))

  ("emacs:prefix b" (interactive go-to-buffer))
  ("emacs:prefix k" (safely-kill-buffer))
  ("emacs:prefix C-c" (safely-quit-TeXmacs))
  ("emacs:prefix C-f" (interactive load-buffer))
  ("emacs:prefix C-s" (save-buffer))
  ("emacs:prefix C-w" (interactive save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-ish commands added by TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("emacs =" (interactive replace-start-forward))

  ("M-a" (make-tree))
  ("M-g" (clipboard-clear "primary"))
  ("M-R" (make-group))
  ("M-[" (undo 0))
  ("M-]" (redo 0))
  ("M-:" (make 'line-break))
  ("M-;" (make 'new-line))
  ("M-return" (make 'next-line))
  ("M-/" (make 'no-break))
  ("M-!" (make-label))
  ("M-?" (make 'reference))
  ("M-C-?" (make 'pageref))

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
  ("pastefrom *" (interactive clipboard-paste)))
