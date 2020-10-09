
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-kbd.scm
;; DESCRIPTION : keyboard handling for graphics mode
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven and Henri Lesourd
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-kbd)
  (:use (generic generic-kbd)
        (utils library cursor)
        (graphics graphics-env)
        (graphics graphics-main)
        (graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-active-graphics?)
  (and (in-graphics?) (== (get-env "preamble") "false")))

(define (in-beamer-graphics?)
  (and (in-active-graphics?) (in-screens?)))

(define (graphics-context? t)
  (tree-is? t 'graphics))

(define (inside-graphics-context? t)
  (tree-search-upwards t graphics-context?))

(define (inside-graphical-text-context? t)
  (and-with p (tree-ref t :up)
    (and-with u (tree-search-upwards p graphical-text-context?)
      (inside-graphics-context? u))))

(tm-define (generic-context? t)
  (:require (inside-graphics-context? t))
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra abbreviations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-zoom-in)
  (graphics-zoom 1.189207115))
(tm-define (graphics-zoom-out)
  (graphics-zoom 0.840896415))

(tm-define (graphics-move-origin-left)
  (graphics-move-origin "+0.01gw" "0gh"))
(tm-define (graphics-move-origin-right)
  (graphics-move-origin "-0.01gw" "0gh"))
(tm-define (graphics-move-origin-down)
  (graphics-move-origin "0gw" "+0.01gh"))
(tm-define (graphics-move-origin-up)
  (graphics-move-origin "0gw" "-0.01gh"))
(tm-define (graphics-move-origin-left-fast)
  (graphics-move-origin "+0.1gw" "0gh"))
(tm-define (graphics-move-origin-right-fast)
  (graphics-move-origin "-0.1gw" "0gh"))
(tm-define (graphics-move-origin-down-fast)
  (graphics-move-origin "0gw" "+0.1gh"))
(tm-define (graphics-move-origin-up-fast)
  (graphics-move-origin "0gw" "-0.1gh"))

(tm-define (graphics-decrease-hsize)
  (graphics-change-extents "-0.1cm" "0cm"))
(tm-define (graphics-increase-hsize)
  (graphics-change-extents "+0.1cm" "0cm"))
(tm-define (graphics-decrease-vsize)
  (graphics-change-extents "0cm" "-0.1cm"))
(tm-define (graphics-increase-vsize)
  (graphics-change-extents "0cm" "+0.1cm"))
(tm-define (graphics-decrease-hsize-fast)
  (graphics-change-extents "-1cm" "0cm"))
(tm-define (graphics-increase-hsize-fast)
  (graphics-change-extents "+1cm" "0cm"))
(tm-define (graphics-decrease-vsize-fast)
  (graphics-change-extents "0cm" "-1cm"))
(tm-define (graphics-increase-vsize-fast)
  (graphics-change-extents "0cm" "+1cm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-active-graphics?)
  ("+" (graphics-zoom-in))
  ("-" (graphics-zoom-out))
  ("0" (graphics-set-zoom 0.5))
  ("1" (graphics-set-zoom 1.0))
  ("2" (graphics-set-zoom 2.0))
  ("3" (graphics-set-zoom 3.0))
  ("4" (graphics-set-zoom 4.0))
  ("5" (graphics-set-zoom 5.0))
  ("6" (graphics-set-zoom 6.0))
  ("7" (graphics-set-zoom 7.0))
  ("8" (graphics-set-zoom 8.0))
  ("9" (graphics-set-zoom 9.0))
  ("#" (graphics-toggle-grid))
  ("!" (open-plots-editor "scheme" "default" ""))
  ("left" (graphics-move-origin-left))
  ("right" (graphics-move-origin-right))
  ("down" (graphics-move-origin-down))
  ("up" (graphics-move-origin-up))
  ("S-left" (graphics-move-origin-left-fast))
  ("S-right" (graphics-move-origin-right-fast))
  ("S-down" (graphics-move-origin-down-fast))
  ("S-up" (graphics-move-origin-up-fast))
  ("home" (graphics-zmove 'foreground))
  ("end" (graphics-zmove 'background))
  ("pageup" (graphics-zmove 'closer))
  ("pagedown" (graphics-zmove 'farther))
  ("return" (graphics-apply-props-at-mouse))
  ("S-return" (graphics-get-props-at-mouse))
  ("A-left" (graphics-decrease-hsize))
  ("A-right" (graphics-increase-hsize))
  ("A-down" (graphics-increase-vsize))
  ("A-up" (graphics-decrease-vsize))
  ("A-S-left" (graphics-decrease-hsize-fast))
  ("A-S-right" (graphics-increase-hsize-fast))
  ("A-S-down" (graphics-increase-vsize-fast))
  ("A-S-up" (graphics-decrease-vsize-fast))
  ("backspace" (graphics-kbd-remove #f))
  ("delete" (graphics-kbd-remove #t))
  ("C-g" (graphics-toggle-logical-grid))
  ("C-G" (graphics-toggle-visual-grid))
  ("C-2" (graphics-set-grid-aspect 'detailed 2 #t))
  ("C-3" (graphics-set-grid-aspect 'detailed 3 #t))
  ("C-4" (graphics-set-grid-aspect 'detailed 4 #t))
  ("C-5" (graphics-set-grid-aspect 'detailed 5 #t))
  ("C-6" (graphics-set-grid-aspect 'detailed 6 #t))
  ("C-7" (graphics-set-grid-aspect 'detailed 7 #t))
  ("C-8" (graphics-set-grid-aspect 'detailed 8 #t))
  ("C-9" (graphics-set-grid-aspect 'detailed 9 #t))
  ("C-0" (graphics-set-grid-aspect 'detailed 10 #t))
  ("C-left" (graphics-rotate-xz -0.1))
  ("C-right" (graphics-rotate-xz 0.1))
  ("C-up" (graphics-rotate-yz 0.1))
  ("C-down" (graphics-rotate-yz -0.1))
  ("C-home" (graphics-zmove 'foreground))
  ("C-end" (graphics-zmove 'background))
  ("C-pageup" (graphics-zmove 'closer))
  ("C-pagedown" (graphics-zmove 'farther)))

(kbd-map
  (:mode in-beamer-graphics?)
  ("pageup" (screens-switch-to :previous))
  ("pagedown" (screens-switch-to :next)))

(define graphics-keys
  '("+" "-" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "#" "!"
    "left" "right" "down" "up" "home" "end" "pageup" "pagedown"
    "return" "backspace" "delete" "tab"
    "F1" "F2" "F3" "F4" "F9" "F10" "F11" "F12"))

(tm-define (keyboard-press key time)
  (:mode in-active-graphics?)
  (cond ((string-occurs? "-" key) (key-press key))
        ((in? key graphics-keys) (key-press key))))

(tm-define (mouse-drop-event x y obj)
  (:mode in-active-graphics?)
  (set! the-graphics-drop-object (tm->stree obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overriding standard structured editing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-variant t forwards?)
  (:require (in-active-graphics?))
  (graphics-choose-point (if forwards? 1 -1)))

(tm-define (graphics-kbd-remove forward?)
  (cond ((and (with-active-selection?)
              (with-cursor (rcons (selection-path) 0)
                (not (in-graphics?))))
         (go-to (rcons (selection-path) 0))
         (clipboard-cut "primary"))
        ((inside-graphical-text?)
         (if forward? (kbd-delete) (kbd-backspace)))
        (else
         (edit_delete))))

(tm-define (geometry-vertical t down?)
  (:require (in-active-graphics?))
  (graphics-change-geo-valign down?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text at 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-horizontal t forwards?)
  (:require (graphical-text-context? t))
  (with-define (move) ((if forwards? go-right go-left))
    (with-define (next) (go-to-next-inside move inside-graphics-context?)
      (go-to-next-such-that next inside-graphical-text-context?))))

(tm-define (kbd-vertical t downwards?)
  (:require (graphical-text-context? t))
  (with-define (move) ((if downwards? go-down go-up))
    (with-define (next) (go-to-next-inside move inside-graphics-context?)
      (go-to-next-such-that next inside-graphical-text-context?))))

(tm-define (kbd-extremal t forwards?)
  (:require (graphical-text-context? t))
  (and-with c (tree-down t)
    (tree-go-to c (if forwards? :end :start))))

(tm-define (geometry-horizontal t forwards?)
  (:require (graphical-text-context? t))
  (let* ((old (graphical-get-attribute t "text-at-halign"))
         (new (if forwards?
                  (cond ((== old "right") "center")
                        (else "left"))
                  (cond ((== old "left") "center")
                        (else "right")))))
    (graphical-set-attribute t "text-at-halign" new)))

(tm-define (geometry-vertical t down?)
  (:require (graphical-text-context? t))
  (let* ((valign-var (graphics-valign-var t))
         (old (graphical-get-attribute t valign-var))
         (new (if down?
                  (cond ((== old "bottom") "base")
                        ((== old "base") "axis")
                        ((== old "axis") "center")
                        (else "top"))
                  (cond ((== old "top") "center")
                        ((== old "center") "axis")
                        ((== old "axis") "base")
                        (else "bottom")))))
    (graphical-set-attribute t valign-var new)))

(tm-define (geometry-extremal t forwards?)
  (:require (graphical-text-context? t))
  (graphical-set-attribute t "text-at-halign"
                           (if forwards? "left" "right")))

(tm-define (geometry-incremental t down?)
  (:require (graphical-text-context? t))
  (graphical-set-attribute t (graphics-valign-var t)
                           (if down? "top" "bottom")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw over / draw under
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode inside-graphical-over-under?)
  ("C-*" (graphics-toggle-over-under)))
