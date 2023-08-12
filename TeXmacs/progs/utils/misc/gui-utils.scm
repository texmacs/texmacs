
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-utils.scm
;; DESCRIPTION : support functions for gui markup in style packages
;; COPYRIGHT   : (C) 2023  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc gui-utils)
  (:use (utils misc tooltip)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-on-select type x y cmd)
  (:secure #t)
  ;;(display* "gui-on-select " type ", " x ", " y ", " cmd "\n")
  (set! type (tm->stree type))
  (set! cmd (tm->stree cmd))
  (or (and (in? type (list "click" "drag")) "done")
      (and (== type "select")
           (begin
             (keyboard-focus-on "canvas")
             (delayed
               (:idle 1)
               (when (string? cmd)
                 (secure-eval (string->object cmd)))
               (close-tooltip))
             (update-menus)
             "done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard emulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define emu-modifier-table (make-ahash-table))

(tm-define (emu-toggle-modifier t)
  (:secure #t)
  (ahash-set! emu-modifier-table t
              (not (ahash-ref emu-modifier-table t)))
  (refresh-now "custom-keyboard"))

(tm-define (emu-active-modifier? t)
  (ahash-ref emu-modifier-table t))

(tm-define (emu-key t)
  (:secure #t)
  (with s (tm->stree t)
    (when (string? s)
      (if (emu-active-modifier? "Control")
          (set! s (string-append "C-" s)))
      (if (emu-active-modifier? "Alt")
          (set! s (string-append "A-" s)))
      (if (emu-active-modifier? "Meta")
          (set! s (string-append "M-" s)))
      (key-press s)
      (if (not (emu-active-modifier? "Lock"))
          (set! emu-modifier-table (make-ahash-table))))))
