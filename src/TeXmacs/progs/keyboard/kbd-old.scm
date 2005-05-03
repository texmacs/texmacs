
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-old.scm
;; DESCRIPTION : keyboard shortcuts for old look and feel
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (keyboard kbd-old)
  (:use
    (texmacs texmacs tm-server) (texmacs texmacs tm-files)
    (utils edit selections)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wildcards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-variant-keys "*" "C-*")

(kbd-wildcards pre
  ("cmd" "M-")
  ("text" "cmd s")
  ("math" "M-")
  ("prog" "M-")
  ("font" "cmd C-")
  ("symbol" "M-M-")
  ("table" "cmd t")
  ("inactive" "cmd i")
  ("executable" "cmd e")
  ("text:symbol" "A-")
  ("selection:1" "M-1")
  ("selection:2" "M-2")
  ("selection:3" "M-3")
  ("selection:any" "M-0")
  ("var" "*" #t)
  ("unvar" "C-*" #t)

  ("emacs" "C-" #t)
  ("emacs:meta" "M-" #t)
  ("emacs:contextual" "emacs c")
  ("emacs:prefix" "emacs x")

  ("accent:tilde" "M-~")
  ("accent:hat" "M-^")
  ("accent:umlaut" "M-\"")
  ("accent:acute" "M-'")
  ("accent:grave" "M-`")
  ("accent:cedilla" "M-C")
  ("accent:breve" "M-U")
  ("accent:check" "M-V")
  ("accent:doubleacute" "M-H")
  ("accent:abovering" "M-O")
  ("accent:abovedot" "M-.")
  ("accent:ogonek" "M-G")

  ("math:greek" "A-" #t)
  ("math:bold" "F5" #t)
  ("math:bold:greek" "math:bold math:greek" #t)
  ("math:bold:cal" "math:bold math:cal" #t)
  ("math:cal" "F6" #t)
  ("math:frak" "F7" #t)
  ("math:bbb" "F8" #t)
  ("math:symbol" "M-M-" #t)
  ("math:symbol:circled" "math:symbol @" #t)
  ("math:symbol:limits" "math L" #t)
  ("math:under" "math u" #t)
  ("math:large" "math:symbol" #t)
  ("math:left" "math:symbol l" #t)
  ("math:middle" "math:symbol m" #t)
  ("math:right" "math:symbol r" #t))

(kbd-wildcards
  ("escape" "M-" #t)
  ("escape escape" "M-M-" #t)
  ("escape escape escape" "noop" #t)
  ("H-" "F5" #t)
  ("H-C-" "F6" #t)
  ("H-C-S-" "S-F6" #t)
  ("H-A-" "F7" #t)
  ("H-M-" "F8" #t)

  ("tilde" "M-~")
  ("hat" "accent:deadhat") ;; needed for dead ^ in math mode
  ("umlaut" "M-\"")
  ("acute" "M-'")
  ("grave" "M-`")
  ("cedilla" "M-C")
  ("breve" "M-U")
  ("check" "M-V")
  ("doubleacute" "M-H")
  ("abovering" "M-O")
  ("abovedot" "M-.")
  ("ogonek" "M-G"))

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

  ("emacs:meta <" (go-start))
  ("emacs:meta >" (go-end))
  ("emacs:meta $" (spell-start))

  ("emacs:prefix k" (kill-buffer))
  ("emacs:prefix C-c" (safely-quit-TeXmacs))
  ("emacs:prefix C-f" (interactive '("File name:") 'load-buffer))
  ("emacs:prefix C-s" (save-buffer))
  ("emacs:prefix C-w" (interactive '("Save as:") 'save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-ish commands added by TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("selection:1" "" "Operation on primary selection")
  ("selection:2" "" "Operation on secondary selection")
  ("selection:3" "" "Operation on ternary selection")
  ("selection:any" "" "Operation on named selection")

  ("emacs =" (interactive '("Replace:" "Replace by:") 'replace-start-forward))

  ("emacs:meta a" (make-tree))
  ("emacs:meta c" (clipboard-copy "primary"))
  ("emacs:meta f" (make 'macro))
  ("emacs:meta g" (make-group))
  ("emacs:meta m" (make 'macro))
  ("emacs:meta p" (clipboard-paste "primary"))
  ("emacs:meta r" (clipboard-clear "primary"))
  ("emacs:meta v" (selection-move))
  ("emacs:meta w" (make 'with 3))
  ("emacs:meta x" (clipboard-cut "primary"))
  ("emacs:meta =" (make 'assign))
  ("emacs:meta *" (interactive '("Command:") 'footer-eval))
  ("emacs:meta [" (undo))
  ("emacs:meta ]" (redo))
  ("emacs:meta :" (make 'line-break))
  ("emacs:meta ;" (make 'new-line))
  ("emacs:meta return" (make 'next-line))
  ("emacs:meta /" (make 'no-break))
  ("emacs:meta !" (make 'label))
  ("emacs:meta ?" (make 'reference))

  ("selection:1 c" (noop) (clipboard-copy "primary"))
  ("selection:1 p" (noop) (clipboard-paste "primary"))
  ("selection:1 x" (noop) (clipboard-cut "primary"))
  ("selection:1 r" (noop) (clipboard-clear "primary"))
  ("selection:2 c" (clipboard-copy "secondary"))
  ("selection:2 p" (clipboard-paste "secondary"))
  ("selection:2 x" (clipboard-cut "secondary"))
  ("selection:2 r" (clipboard-clear "secondary"))
  ("selection:3 c" (clipboard-copy "ternary"))
  ("selection:3 p" (clipboard-paste "ternary"))
  ("selection:3 x" (clipboard-cut "ternary"))
  ("selection:3 r" (clipboard-clear "ternary"))
  ("selection:any c" (interactive '("Copy to:") 'clipboard-copy))
  ("selection:any p" (interactive '("Paste from:") 'clipboard-paste))
  ("selection:any x" (interactive '("Cut to:") 'clipboard-cut))
  ("selection:any r" (interactive '("Clear selection:") 'clibpoard-clear)))
