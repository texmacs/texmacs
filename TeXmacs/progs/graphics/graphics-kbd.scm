
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
  (:use (utils library cursor) (utils library tree)
        (graphics graphics-utils) (graphics graphics-main)
        (graphics graphics-env) (graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ShiftMask     256)
(tm-define LockMask      512)
(tm-define ControlMask  1024)
(tm-define Mod1Mask     2048)
(tm-define Mod2Mask     4096)
(tm-define Mod3Mask     8192)
(tm-define Mod4Mask    16384)
(tm-define Mod5Mask    32768)

(tm-define (kbd-tab)
  (:mode in-graphics?)
  (graphics-choose-point))

(tm-define (kbd-left)
  (:inside text-at)
  (go-to-remain-inside go-left 'text-at))

(tm-define (kbd-right)
  (:inside text-at)
  (go-to-remain-inside go-right 'text-at))

(tm-define (kbd-up)
  (:inside text-at)
  (go-to-remain-inside go-up 'text-at))

(tm-define (kbd-down)
  (:inside text-at)
  (go-to-remain-inside go-down 'text-at))

(tm-define (kbd-start-line)
  (:inside text-at)
  (with move (lambda () (go-to-remain-inside go-left 'text-at))
    (go-to-repeat move)))

(tm-define (kbd-end-line)
  (:inside text-at)
  (with move (lambda () (go-to-remain-inside go-right 'text-at))
    (go-to-repeat move)))

(define (in-active-graphics?)
  (and (in-graphics?) (== (get-env "preamble") "false")))

(define (graphics-kbd-remove forward?)
  (if (and (with-active-selection?)
	   (with-cursor (rcons (selection-path) 0)
	     (not (in-graphics?))))
      (begin
	(go-to (rcons (selection-path) 0))
	(clipboard-cut "primary"))))

(kbd-map
  (:mode in-active-graphics?)
  ("+" (graphics-zoom (/ 1.0 0.75)))
  ("-" (graphics-zoom 0.75))
  ("left" (graphics-move-origin "+0.01gw" "0gh"))
  ("right" (graphics-move-origin "-0.01gw" "0gh"))
  ("down" (graphics-move-origin "0gw" "+0.01gh"))
  ("up" (graphics-move-origin "0gw" "-0.01gh"))
  ("S-left" (graphics-move-origin "+0.1gw" "0gh"))
  ("S-right" (graphics-move-origin "-0.1gw" "0gh"))
  ("S-down" (graphics-move-origin "0gw" "+0.1gh"))
  ("S-up" (graphics-move-origin "0gw" "-0.1gh"))
  ("home" (graphics-zmove 'foreground))
  ("end" (graphics-zmove 'background))
  ("pageup" (graphics-zmove 'closer))
  ("pagedown" (graphics-zmove 'farther))
  ("A-left" (graphics-change-extents "-0.1cm" "0cm"))
  ("A-right" (graphics-change-extents "+0.1cm" "0cm"))
  ("A-down" (graphics-change-extents "0cm" "+0.1cm"))
  ("A-up" (graphics-change-extents "0cm" "-0.1cm"))
  ("A-S-left" (graphics-change-extents "-1cm" "0cm"))
  ("A-S-right" (graphics-change-extents "+1cm" "0cm"))
  ("A-S-down" (graphics-change-extents "0cm" "+1cm"))
  ("A-S-up" (graphics-change-extents "0cm" "-1cm"))
  ("M-left"  (if (current-is-textat?)
		 (text-at-change-halign current-path #f)))
  ("M-right" (if (current-is-textat?)
		 (text-at-change-halign current-path #t)))
  ("M-down"  (if (current-is-textat?)
		 (text-at-change-valign current-path #f)
		 (graphics-change-geo-valign #f)))
  ("M-up"    (if (current-is-textat?)
		 (text-at-change-valign current-path #t)
		 (graphics-change-geo-valign #t)))
  ("backspace" (graphics-kbd-remove #f))
  ("delete" (graphics-kbd-remove #t))
  ("C-g" (graphics-toggle-grid #f))
  ("C-G" (graphics-toggle-grid #t)))

(tm-define (inside-draw-over/under?)
  (or (inside? 'draw-over) (inside? 'draw-under)))

(tm-define (graphics-toggle-draw-over/under)
  (with-innermost t (lambda (x) (tree-in? x '(draw-over draw-under)))
    (if (tree-is? t 'draw-over)
	(begin
	  (tree-assign-node! t 'draw-under)
	  (tree-go-to t 0 :end))
	(begin
	  (tree-assign-node! t 'draw-over)
	  (if (tree-is? (tree-ref t 1) 'with)
	      (tree-go-to t 1 (- (tree-arity (tree-ref t 1)) 1) :end)
	      (tree-go-to t 1 :end))))))

(kbd-map
  (:mode inside-draw-over/under?)
  ("C-*" (graphics-toggle-draw-over/under)))
