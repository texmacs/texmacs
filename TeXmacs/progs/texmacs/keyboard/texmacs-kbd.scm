
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-kbd.scm
;; DESCRIPTION : server-related keyboard shortcuts for all modes
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs keyboard texmacs-kbd)
  (:use
    (texmacs texmacs tm-files)
    (texmacs texmacs tm-print)
    (doc help-funcs)))

(kbd-map
  ("F1" (interactive docgrep-in-doc))
  ("S-F1" (load-help-buffer "about/welcome/welcome"))
  ("F2" (choose-file load-buffer "Load file" ""))
  ("S-F2" (choose-file load-in-new-window "Load file" ""))
  ("C-F2" (revert-buffer))
  ("M-F2" (new-buffer))
  ("M-S-F2" (open-window))
  ("M-C-F2" (clone-window))
  ("F3" (save-buffer))
  ("S-F3" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ("F4" (preview-with-ghostview))
  ("S-F4" (print))
  ("C-F4" (interactive print-to-file))
  ("M-F4" (interactive print-pages))
  ("M-S-F4" (interactive print-pages-to-file))
  ("C-F9" (toggle-full-screen-mode))

  ("undo" (noop) (undo 0))
  ("redo" (noop) (redo 0))
  ("cancel" (noop) (clipboard-clear "primary"))
  ("cut" (noop) (clipboard-cut "primary"))
  ("paste" (noop) (clipboard-paste "primary"))
  ("copy" (noop) (clipboard-copy "primary"))
  ("find" (noop) (search-start #t)))
