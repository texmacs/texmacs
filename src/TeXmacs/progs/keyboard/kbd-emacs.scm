
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-emacs.scm
;; DESCRIPTION : keyboard shortcuts for emacs look and feel
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (keyboard kbd-emacs)
  (:use
    (texmacs texmacs tm-server) (texmacs texmacs tm-files)
    (texmacs edit edit-misc) (texmacs tools tm-select)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wildcards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-variant-keys "tab" "S-tab")

(kbd-wildcards pre
  ("cmd" "M-")
  ("font" "M-A-")
  ("text" "A-")
  ("math" "A-")
  ("prog" "A-")
  ("symbol" "S-F5" #t)
  ("table" "cmd t")
  ("inactive" "cmd i")
  ("executable" "cmd e")
  ("text:symbol" "S-F5" #t)
  ("var" "tab" #t)
  ("unvar" "S-tab" #t)

  ("emacs" "C-" #t)
  ("emacs:meta" "M-" #t)
  ("emacs:contextual" "emacs c")
  ("emacs:prefix" "emacs x")
  ("emacs:copyto" "emacs:meta W")
  ("emacs:cutto" "emacs W")
  ("emacs:pastefrom" "emacs Y")
  
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
  ("A-C-" "H-" #t)
  ("H-" "F5" #t)
  ("H-C-" "F6" #t)
  ("H-C-S-" "S-F6" #t)
  ("H-A-" "F7" #t)
  ("H-M-" "F8" #t)

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
;; Standard Emacs commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("emacs" "" "Emacs command")
  ("emacs:contextual" "" "Emacs mode specific prefix command")
  ("emacs:prefix" "" "Emacs prefix command")

  ("emacs a" (go-start-line))
  ("emacs b" (go-left))
  ("emacs d" (remove-text #t))
  ("emacs e" (go-end-line))
  ("emacs f" (go-right))
  ("emacs g" (selection-cancel))
  ("emacs j" (insert-return))
  ("emacs k" (kill-line))
  ("emacs l" (recenter-window))
  ("emacs m" (insert-return))
  ("emacs n" (go-down))
  ("emacs o" (open-line))
  ("emacs p" (go-up))
  ("emacs q" (make 'symbol))
  ("emacs r" (search-start #f))
  ("emacs s" (search-start #t))
  ("emacs t" (transpose-chars))
  ("emacs v" (go-page-up))
  ("emacs w" (clipboard-cut "primary"))
  ("emacs y" (clipboard-paste "primary"))
  ("emacs insert" (clipboard-copy "primary"))
  ("emacs \\" (toggle-input-method))
  ("emacs ]" (abort-recursive-edit))
  ("emacs _" (undo))
  ("emacs /" (undo))
  ("emacs space" (kbd-select-enlarge))
  ("emacs tab" (kbd-select-environment))

  ("emacs:meta w" (clipboard-copy "primary"))
  ("emacs:meta x" (interactive '("Command:") 'footer-eval))
  ("emacs:meta <" (go-start))
  ("emacs:meta >" (go-end))
  ("emacs:meta $" (spell-start))

  ("emacs:prefix k" (safely-kill-buffer))
  ("emacs:prefix C-c" (safely-quit-TeXmacs))
  ("emacs:prefix C-f" (interactive '("File name:") 'load-buffer))
  ("emacs:prefix C-s" (save-buffer))
  ("emacs:prefix C-w" (interactive '("Save as:") 'save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-ish commands added by TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("emacs:copyto" "" "Copy to (1, 2, 3, *:other)")
  ("emacs:cutto" "" "Cut to (1, 2, 3, *:other)")
  ("emacs:pastefrom" "" "Paste from (1, 2, 3, *:other)")

  ("emacs =" (interactive '("Replace:" "Replace by:") 'replace-start-forward))

  ("emacs:meta a" (make-tree))
  ("emacs:meta g" (clipboard-clear "primary"))
  ("emacs:meta R" (make-group))
  ("emacs:meta [" (undo))
  ("emacs:meta ]" (redo))
  ("emacs:meta :" (make 'line-break))
  ("emacs:meta ;" (make 'new-line))
  ("emacs:meta return" (make 'next-line))
  ("emacs:meta /" (make 'no-break))
  ("emacs:meta !" (make 'label))
  ("emacs:meta ?" (make 'reference))
  ("emacs:meta C-?" (make 'pageref))

  ("emacs:copyto 1" (noop) (clipboard-copy "primary"))
  ("emacs:copyto 2" (clipboard-copy "secondary"))
  ("emacs:copyto 3" (clipboard-copy "ternary"))
  ("emacs:copyto *" (interactive '("Copy to:") 'clipboard-copy))
  ("emacs:cutto 1" (noop) (clipboard-cut "primary"))
  ("emacs:cutto 2" (clipboard-cut "secondary"))
  ("emacs:cutto 3" (clipboard-cut "ternary"))
  ("emacs:cutto *" (interactive '("Cut to:") 'clipboard-cut))
  ("emacs:pastefrom 1" (noop) (clipboard-paste "primary"))
  ("emacs:pastefrom 2" (clipboard-paste "secondary"))
  ("emacs:pastefrom 3" (clipboard-paste "ternary"))
  ("emacs:pastefrom *" (interactive '("Paste from:") 'clipboard-paste)))
