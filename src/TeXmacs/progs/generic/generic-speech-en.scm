
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-speech-en.scm
;; DESCRIPTION : generic editing using English speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-speech-en)
  (:use (generic generic-speech)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General speech commands for all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(speech-map english any
  ("period" ".")
  ("comma" ",")
  ("colon" ":")
  ("semicolon" ";")
  ("exclamation mark" "!")
  ("question mark" "?")

  ("enter" (kbd-return))
  ("return" (kbd-return))
  ("new line" (kbd-return))
  ("new paragraph" (kbd-return))
  ("delete" (kbd-delete))
  ("backspace" (kbd-backspace))
  ("left" (kbd-left))
  ("right" (kbd-right))
  ("up" (kbd-up))
  ("down" (kbd-down))
  ("page up" (kbd-page-up))
  ("page down" (kbd-page-down))
  ("start line" (kbd-start-line))
  ("end line" (kbd-end-line))
  ("start document" (go-start))
  ("end document" (go-end))

  ("delete tag" (remove-structure-upwards))
  ("delete inner" (remove-structure-upwards))
  ("delete innermost" (remove-structure-upwards))
  ("delete structure" (remove-structure-upwards))
  ("delete forwards" (structured-remove-right))
  ("delete backwards" (structured-remove-left))
  ("delete upwards" (structured-remove-up))
  ("delete downwards" (structured-remove-up))
  ("insert left" (structured-insert-left))
  ("insert right" (structured-insert-right))
  ("insert up" (structured-insert-up))
  ("insert down" (structured-insert-down))

  ("leave" (speech-leave))
  ("leave leave" (speech-leave) (speech-leave))
  ("leave leave leave" (speech-leave) (speech-leave) (speech-leave))
  ("leave right" (speech-leave))
  ("leave left" (structured-exit-left))
  ("exit" (speech-leave))
  ("exit right" (speech-leave))
  ("exit left" (structured-exit-left))

  ("undo" (undo 0))
  ("redo" (redo 0))
  ("undo undo" (undo 0) (undo 0))
  ("redo redo" (redo 0) (redo 0))
  ("undo undo undo" (undo 0) (undo 0) (undo 0))
  ("redo redo redo" (redo 0) (redo 0) (redo 0))
  ("undo undo undo undo" (undo 0) (undo 0) (undo 0) (undo 0))
  ("redo redo redo redo" (redo 0) (redo 0) (redo 0) (redo 0))
  ("undo undo undo undo undo" (undo 0) (undo 0) (undo 0) (undo 0) (undo 0))
  ("redo redo redo redo redo" (redo 0) (redo 0) (redo 0) (redo 0) (redo 0))
  ("cancel" (kbd-cancel))
  ("cut" (kbd-cut))
  ("paste" (kbd-paste))
  ("copy" (kbd-copy))
  ("search" (interactive-search))
  ("previous match" (search-next-match #f))
  ("next match" (search-next-match #t))
  ("spell" (interactive-spell))
  ("replace" (interactive-replace))
  
  ("back" (cursor-history-backward))
  ("forward" (cursor-history-backward))
  ("number" (numbered-toggle (focus-tree)))
  ("toggle" (alternate-toggle (focus-tree)))
  ("zoom in" (zoom-in (sqrt (sqrt 2.0))))
  ("zoom out" (zoom-out (sqrt (sqrt 2.0))))
  ("reset zoom" (change-zoom-factor 1.0))
  
  ("fit all to screen" (fit-all-to-screen))
  ("fit to screen" (fit-to-screen))
  ("fit to screen width" (fit-to-screen-width)))
