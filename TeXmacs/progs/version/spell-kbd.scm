
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : spell-kbd.scm
;; DESCRIPTION : keyboard shortcuts for spell checking
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version spell-kbd)
  (:use (generic generic-kbd)
        (version spell-edit)
        (utils misc lantool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-spell?)
  ("C-up" (spell-go-to-previous))
  ("C-down" (spell-go-to-next))
  ("C-home" (spell-go-to-first))
  ("C-pageup" (spell-go-to-previous))
  ("C-pagedown" (spell-go-to-next))
  ("C-end" (spell-go-to-last))
  ("1" (spell-retain 1 #f))
  ("2" (spell-retain 2 #f))
  ("3" (spell-retain 3 #f))
  ("4" (spell-retain 4 #f))
  ("5" (spell-retain 5 #f))
  ("6" (spell-retain 6 #f))
  ("7" (spell-retain 7 #f))
  ("8" (spell-retain 8 #f))
  ("9" (spell-retain 9 #f))
  ("C-1" (spell-retain 1 #t))
  ("C-2" (spell-retain 2 #t))
  ("C-3" (spell-retain 3 #t))
  ("C-4" (spell-retain 4 #t))
  ("C-5" (spell-retain 5 #t))
  ("C-6" (spell-retain 6 #t))
  ("C-7" (spell-retain 7 #t))
  ("C-8" (spell-retain 8 #t))
  ("C-9" (spell-retain 9 #t))
  ("+" (spell-retain-permanent))
  ("C-c" (spell-terminate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special key shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-left)
  (:mode in-spell?)
  (go-to-remain-inside go-left spell-context? 0))

(tm-define (kbd-right)
  (:mode in-spell?)
  (go-to-remain-inside go-right spell-context? 0))

(tm-define (kbd-up)
  (:mode in-spell?)
  (spell-go-to-previous))

(tm-define (kbd-down)
  (:mode in-spell?)
  (spell-go-to-next))

(tm-define (kbd-start-line)
  (:mode in-spell?)
  (with-innermost t spell-context?
    (tree-go-to t 0 :start)))

(tm-define (kbd-page-up)
  (:mode in-spell?)
  (spell-go-to-previous))

(tm-define (kbd-page-down)
  (:mode in-spell?)
  (spell-go-to-next))

(tm-define (kbd-end-line)
  (:mode in-spell?)
  (with-innermost t spell-context?
    (tree-go-to t 0 :end)))

(tm-define (kbd-return)
  (:mode in-spell?)
  (spell-retain 0 #f))

(tm-define (kbd-control-return)
  (:mode in-spell?)
  (spell-retain 0 #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (keyboard-press key time)
  (:require (inside-spell?))
  (with-innermost old-t spell-context?
    (let* ((old-p (tree->path old-t))
           (old-t* (tm->stree old-t)))           
      (former key time)
      (with-innermost new-t spell-context?
        (let* ((new-p (tree->path new-t))
               (new-t* (tm->stree new-t)))
          (when (and (!= old-t* new-t*) (== old-p new-p))
            (lantool-recheck)))))))
