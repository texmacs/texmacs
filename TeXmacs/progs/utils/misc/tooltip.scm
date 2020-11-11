
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tooltip.scm
;; DESCRIPTION : tooltip windows
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc tooltip))

(define tooltip-id #f)
(define tooltip-win #f)
(define tooltip-unmap? #f)
(define tooltip-settings #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map and unmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tooltip-confirm settings)
  ;;(display* "Confirm " tooltip-win "\n")
  (set! tooltip-unmap? #f)
  (set! tooltip-settings settings))

(define (tooltip-unmap)
  ;;(display* "Unmap " tooltip-win "\n")
  (alt-window-hide tooltip-win)
  (alt-window-delete tooltip-win)
  (set! tooltip-id #f)
  (set! tooltip-win #f)
  (set! tooltip-unmap? #f)
  (set! tooltip-settings #f))

(define (tooltip-map wid x y id settings)
  (set! x (quotient x 256))
  (set! y (quotient y 256))
  (if tooltip-win (tooltip-unmap))
  (with win (alt-window-handle)
    (alt-window-create-tooltip win wid (translate "Tooltip"))
    (alt-window-set-position win x y)
    (alt-window-show win)
    (set! tooltip-id id)
    (set! tooltip-win win)
    (set! tooltip-unmap? #f)
    (set! tooltip-settings settings)
    ;;(display* "Map " tooltip-win "\n")
    ))

(define (tooltip-delayed-unmap)
  (set! tooltip-unmap? tooltip-win)
  ;;(display* "Schedule unmap " tooltip-win "\n")
  (delayed
    (:pause 250)
    (when (and tooltip-unmap? (== tooltip-unmap? tooltip-win))
      (tooltip-unmap))))
  
(tm-define (keyboard-press key time)
  (:require (and tooltip-win (not tooltip-unmap?)))
  (when (== (cAr tooltip-settings) "keyboard")
    (tooltip-delayed-unmap))
  (former key time))

(tm-define (mouse-event key x y mods time)
  (:require (and tooltip-win (not tooltip-unmap?)))
  ;;(display* "Mouse event " key ", " x ", " y "; " time "\n")
  (with (x1 y1 x2 y2 mx my sx sy zf type) tooltip-settings
    (let* ((xx (inexact->exact (round (/ (- x sx) (/ 5.0 zf)))))
           (yy (inexact->exact (round (/ (- y sy) (/ 5.0 zf)))))
           (dx (quotient (abs (- xx mx)) 256))
           (dy (quotient (abs (- yy my)) 256))
           (d  (* 5 256)))
      (when (or (!= key "move")
                (< xx (- x1 d)) (> xx (+ x2 d))
                (< yy (- y1 d)) (> yy (+ y2 d))
                (> dx 10) (> dy 10))
        (when (!= type "keyboard")
          (tooltip-delayed-unmap)))))
  (former key x y mods time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positioning of the tooltip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tooltip-x x1 x2 wx bw sw mx ha)
  (with pw (* 12 256)
    (cond ((not (string-starts? ha "raw-"))
           (with x (tooltip-x x1 x2 wx bw sw mx (string-append "raw-" ha))
             (max 0 (min (- sw bw) x))))
          ((== ha "raw-Left") (+ wx x1 (- bw)))
          ((== ha "raw-left") (+ wx x1))
          ((== ha "raw-center") (+ wx (quotient (- (+ x1 x2) bw) 2)))
          ((== ha "raw-right") (+ wx x2 (- bw)))
          ((== ha "raw-Right") (+ wx x2))
          ((== ha "raw-mouse-Left") (+ wx mx (- bw)))
          ((== ha "raw-mouse-left") (+ wx mx (- bw) pw))
          ((== ha "raw-mouse-center") (+ wx mx (quotient (- pw bw) 2)))
          ((== ha "raw-mouse-right") (+ wx mx))
          ((== ha "raw-mouse-Right") (+ wx mx pw))
          (else (+ wx x1)))))

(define (tooltip-y y1 y2 wy bh sh my va)
  (let* ((ph (* 20 256))
         (d  (* 5  256)))
    (cond ((string-starts? va "prefer-")
           (let* ((va* (string-drop va 7))
                  (vai (cond ((== va* "Bottom") "Top")
                             ((== va* "Top") "Bottom")
                             ((== va* "mouse-Bottom") "mouse-Top")
                             ((== va* "mouse-Top") "mouse-Bottom")
                             (else va*)))
                  (va1 (string-append "raw-" va*))
                  (va2 (string-append "raw-" vai)))
             (with y (tooltip-y y1 y2 wy bh sh my va1)
               (if (and (>= y (- bh sh)) (<= y 0)) y
                   (tooltip-y y1 y2 wy bh sh my va2)))))
          ((not (string-starts? va "raw-"))
           (with y (tooltip-y y1 y2 wy bh sh my (string-append "raw-" va))
             (max (- bh sh) (min 0 y))))
          ((== va "raw-Bottom") (+ wy y1 (- d)))
          ((== va "raw-bottom") (+ wy y1 bh))
          ((== va "raw-center") (+ wy (quotient (+ (+ y1 y2) bh) 2)))
          ((== va "raw-top") (+ wy y2))
          ((== va "raw-Top") (+ wy y2 bh d))
          ((== va "raw-mouse-Bottom") (+ wy my (- ph)))
          ((== va "raw-mouse-bottom") (+ wy my))
          ((== va "raw-mouse-center") (+ wy my (quotient (- bh ph) 2)))
          ((== va "raw-mouse-top") (+ wy my bh (- ph)))
          ((== va "raw-mouse-Top") (+ wy my bh))
          (else (+ wy y1 (- d))))))

(define (tooltip-position x1 y1 x2 y2 wx wy bsz ssz mpos ha va type)
  (with (bw bh) bsz
    (with (sw sh) ssz
      (with (mx my) mpos
        (cond ((== type "mouse")
               (tooltip-position x1 y1 x2 y2 wx wy bsz ssz mpos
                                 (string-append "mouse-" ha)
                                 (string-append "mouse-" va)
                                 "default"))
              ((or (!= ha "auto") (!= va "auto"))
               (list (tooltip-x x1 x2 wx bw sw mx ha)
                     (tooltip-y y1 y2 wy bh sh my va)))
              ((or (< bw (- x2 x1)) (< bh (- y2 y1)))
               (tooltip-position x1 y1 x2 y2 wx wy bsz ssz mpos
                                 "mouse-right" "prefer-mouse-Bottom" type))
              (else
               (tooltip-position x1 y1 x2 y2 wx wy bsz ssz mpos
                                 "left" "prefer-Bottom" type)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tooltip-init? p)
  (and-with x (and (pair? p) (car p))
    (and (string? x)
         (not (string-starts? x "page-"))
         (nin? x (list "zoom-factor" "full-screen-mode")))))

(tm-define (show-tooltip id ref tip ha va type magf)
  (with (x1 y1 x2 y2) (tree-bounding-rectangle ref)
    (let* ((d (* 5 256))
           (mpos (get-mouse-position))
           (mx (car  mpos))
           (my (cadr mpos))
           (settings (list (- x1 (get-canvas-x))
                           (- y1 (get-canvas-y))
                           (- x2 (get-canvas-x))
                           (- y2 (get-canvas-y))
                           (- mx (get-canvas-x))
                           (- my (get-canvas-y))
                           (get-scroll-x)
                           (get-scroll-y)
                           (get-window-zoom-factor)
                           type)))
      (when (or (== type "keyboard")
                (and (>= mx (- x1 d)) (<= mx (+ x2 d))
                     (>= my (- y1 d)) (<= my (+ y2 d))))
        (if (and tooltip-win id (== id tooltip-id))
            (tooltip-confirm settings)
            (and-let* ((wx (get-window-x))
                       (wy (get-window-y))
                       (packs (get-style-list))
                       (pre (document-get-preamble (buffer-tree)))
                       (zf (get-window-zoom-factor))
                       (mag (number->string (* zf magf)))
                       (inits* (map cdr (cdr (tm->stree (get-all-inits)))))
                       (inits (list-filter inits* tooltip-init?))
                       (env* (apply append inits))
                       (env (append env* (list "magnification" mag)))
                       (doc `(surround (hide-preamble ,pre) "" ,tip))
                       (master (url->system (current-buffer)))
                       (w (widget-texmacs-output
                           `(with ,@env "project" ,master ,doc)
                           `(style (tuple ,@packs))))
                       (bsz (texmacs-widget-size w))
                       (ssz (get-screen-size))
                       (pos (tooltip-position x1 y1 x2 y2 wx wy
                                              bsz ssz mpos ha va type)))
              (tooltip-map w
                           (car pos)
                           (cadr pos)
                           id
                           settings)))))))
