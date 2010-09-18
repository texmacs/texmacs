
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ("pastefrom *" (interactive clipboard-paste)))

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
  ("emacs:prefix C-w" (interactive save-buffer))

  ;; extra Emacs-ish shortcuts
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
  ("M-C-?" (make 'pageref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile macos)

  ;; standard Mac OS keyboard shortcuts
  ("macos c" (clipboard-copy "primary"))
  ("macos f" (search-start #t))
  ("macos n" (new-buffer))
  ("macos o" (choose-file load-buffer "Load file" ""))
  ("macos p" (preview-with-ghostview))
  ("macos q" (safely-quit-TeXmacs))
  ("macos r" (interactive replace-start-forward))
  ("macos s" (save-buffer))
  ("macos S" (choose-file save-buffer "Save TeXmacs file" "texmacs"))
  ("macos v" (clipboard-paste "primary"))
  ("macos w" (safely-kill-buffer))
  ("macos x" (clipboard-cut "primary"))
  ("macos z" (undo 0))
  ("macos Z" (redo 0))
  ("macos ;" (spell-start))

  ;; extra MacOS-like shortcuts
  ("macos F" (search-start #f))

  ("C-g" (selection-cancel))
  ("C-k" (kill-paragraph))
  ("C-q" (make 'symbol))
  ("C-space" (kbd-select-enlarge))

  ("M-a" (make-tree))
  ("M-R" (make-group))
  ("M-x" (interactive exec-interactive-command))
  ("M-X" (interactive footer-eval))
  ("M-:" (make 'line-break))
  ("M-;" (make 'new-line))
  ("M-return" (make 'next-line))
  ("M-/" (make 'no-break))
  ("M-!" (make-label))
  ("M-?" (make 'reference))
  ("M-C-?" (make 'pageref))
  ("M-<" (go-start))
  ("M->" (go-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnome and windows keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:profile gnome windows)

  ;; standard Window shortcuts
  ("windows c" (clipboard-copy "primary"))
  ("windows f" (search-start #t))
  ("windows n" (new-buffer))
  ("windows o" (choose-file load-buffer "Load file" ""))
  ("windows p" (preview-with-ghostview))
  ("windows q" (safely-quit-TeXmacs))
  ("windows r" (interactive replace-start-forward))
  ("windows s" (save-buffer))
  ("windows v" (clipboard-paste "primary"))
  ("windows w" (safely-kill-buffer))
  ("windows x" (clipboard-cut "primary"))
  ("windows z" (undo 0))

  ;; extra Window-ish shortcuts
  ("windows g" (selection-cancel))
  ("windows k" (kill-paragraph))
  ("windows F" (search-start #f))
  ("windows space" (kbd-select-enlarge))

  ("M-a" (make-tree))
  ("M-g" (clipboard-clear "primary"))
  ("M-q" (make 'symbol))
  ("M-x" (interactive footer-eval))
  ("M-R" (make-group))
  ("M-<" (go-start))
  ("M->" (go-end))
  ("M-:" (make 'line-break))
  ("M-;" (make 'new-line))
  ("M-return" (make 'next-line))
  ("M-/" (make 'no-break))
  ("M-!" (make-label))
  ("M-?" (make 'reference))
  ("M-C-?" (make 'pageref))
  ("M-$" (spell-start)))
