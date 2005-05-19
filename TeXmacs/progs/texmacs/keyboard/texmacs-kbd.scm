
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-kbd.scm
;; DESCRIPTION : server-related keyboard shortcuts for all modes
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  ("F2" (choose-file "Load file" "" 'load-buffer))
  ("S-F2" (choose-file "Load file" "" 'load-in-new-window))
  ("C-F2" (revert-buffer))
  ("M-F2" (new-buffer))
  ("M-S-F2" (open-window))
  ("M-C-F2" (clone-window))
  ("F3" (save-buffer))
  ("S-F3" (choose-file "Save TeXmacs file" "texmacs" 'save-buffer))
  ("F4" (print))
  ("S-F4" (interactive print-to-file))
  ("C-F4" (preview-with-ghostview))
  ("M-F4" (interactive print-pages))
  ("M-S-F4" (interactive print-pages-to-file))

  ("undo" (noop) (undo))
  ("redo" (noop) (redo))
  ("cancel" (noop) (clipboard-clear "primary"))
  ("cut" (noop) (clipboard-cut "primary"))
  ("paste" (noop) (clipboard-paste "primary"))
  ("copy" (noop) (clipboard-copy "primary"))
  ("find" (noop) (search-start #t)))
