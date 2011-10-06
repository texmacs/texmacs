
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
        (graphics graphics-env)
	(graphics graphics-main)
	(graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-active-graphics?)
  (and (in-graphics?) (== (get-env "preamble") "false")))

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
;; Keyboard handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ("backspace" (graphics-kbd-remove #f))
  ("delete" (graphics-kbd-remove #t))
  ("C-g" (graphics-toggle-grid #f))
  ("C-G" (graphics-toggle-grid #t)))

(define graphics-keys
  '("+" "-"
    "left" "right" "down" "up" "home" "end" "pageup" "pagedown"
    "backspace" "delete"))

(tm-define (keyboard-press key time)
  (:mode in-active-graphics?)
  (cond ((string-occurs? "-" key) (key-press key))
        ((in? key graphics-keys) (key-press key))))

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
  (with-define (move) ((if forwards? go-right go-left))
    (with-define (next) (go-to-next-inside move inside-graphics-context?)
      (with-define (action)
          (go-to-next-such-that next inside-graphical-text-context?)
        (go-to-repeat action)))))

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
  (let* ((old (graphical-get-attribute t "text-at-valign"))
         (new (if down?
                  (cond ((== old "bottom") "base")
                        ((== old "base") "axis")
                        ((== old "axis") "center")
                        (else "top"))
                  (cond ((== old "top") "center")
                        ((== old "center") "axis")
                        ((== old "axis") "base")
                        (else "bottom")))))
    (graphical-set-attribute t "text-at-valign" new)))

(tm-define (geometry-extremal t forwards?)
  (:require (graphical-text-context? t))
  (graphical-set-attribute t "text-at-halign"
                           (if forwards? "left" "right")))

(tm-define (geometry-incremental t down?)
  (:require (graphical-text-context? t))
  (graphical-set-attribute t "text-at-valign"
                           (if down? "top" "bottom")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw over / draw under
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
