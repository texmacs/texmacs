
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-windows.scm
;; DESCRIPTION : keyboard shortcuts for windows and kde look and feel
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (keyboard kbd-windows)
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

  ("windows" "C-" #t)
  ("windows:meta" "M-" #t)
  ("windows:copyto" "windows C")
  ("windows:cutto" "windows X")
  ("windows:pastefrom" "windows V")

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
;; Standard Windows commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("windows" "" "Windows command")

  ("windows c" (clipboard-copy "primary"))
  ("windows f" (search-start #t))
  ("windows n" (new-buffer))
  ("windows o" (choose-file "Load file" "" 'load-buffer))
  ("windows p" (preview-with-ghostview))
  ("windows q" (safely-quit-TeXmacs))
  ("windows r" (interactive '("Replace:" "Replace by:")'replace-start-forward))
  ("windows s" (save-buffer))
  ("windows v" (clipboard-paste "primary"))
  ("windows w" (safely-kill-buffer))
  ("windows x" (clipboard-cut "primary"))
  ("windows z" (undo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows-ish commands added by TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  ("windows:copyto" "" "Copy to (1, 2, 3, s:search, r:replace, *:other)")
  ("windows:cutto" "" "Cut to (1, 2, 3, s:search, r:replace,*:other)")
  ("windows:pastefrom" "" "Paste from (1, 2, 3, s:search, r:replace, *:other)")

  ("windows g" (selection-cancel))
  ("windows k" (kill-line))
  ("windows F" (search-start #f))
  ("windows space" (kbd-select-enlarge))

  ("windows:meta a" (make-tree))
  ("windows:meta g" (clipboard-clear "primary"))
  ("windows:meta q" (make 'symbol))
  ("windows:meta x" (interactive '("Command:") 'footer-eval))
  ("windows:meta R" (make-group))
  ("windows:meta <" (go-start))
  ("windows:meta >" (go-end))
  ("windows:meta :" (make 'line-break))
  ("windows:meta ;" (make 'new-line))
  ("windows:meta return" (make 'next-line))
  ("windows:meta /" (make 'no-break))
  ("windows:meta !" (make 'label))
  ("windows:meta ?" (make 'reference))
  ("windows:meta C-?" (make 'pageref))
  ("windows:meta $" (spell-start))

  ("windows:copyto 1" (noop) (clipboard-copy "primary"))
  ("windows:copyto 2" (clipboard-copy "secondary"))
  ("windows:copyto 3" (clipboard-copy "ternary"))
  ("windows:copyto s" (clipboard-copy "search"))
  ("windows:copyto r" (clipboard-copy "replace"))
  ("windows:copyto *" (interactive '("Copy to:") 'clipboard-copy))
  ("windows:cutto 1" (noop) (clipboard-cut "primary"))
  ("windows:cutto 2" (clipboard-cut "secondary"))
  ("windows:cutto 3" (clipboard-cut "ternary"))
  ("windows:cutto s" (clipboard-cut "search"))
  ("windows:cutto r" (clipboard-cut "replace"))
  ("windows:cutto *" (interactive '("Cut to:") 'clipboard-cut))
  ("windows:pastefrom 1" (noop) (clipboard-paste "primary"))
  ("windows:pastefrom 2" (clipboard-paste "secondary"))
  ("windows:pastefrom 3" (clipboard-paste "ternary"))
  ("windows:pastefrom s" (clipboard-paste "search"))
  ("windows:pastefrom r" (clipboard-paste "replace"))
  ("windows:pastefrom *" (interactive '("Paste from:") 'clipboard-paste)))
