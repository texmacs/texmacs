
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics.scm
;; DESCRIPTION : editing routines for graphics mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-graphics)
  (:export
    ;; making graphics and setting global graphics properties
    make-graphics
    graphics-set-property graphics-remove-property
    graphics-set-unit graphics-set-unit-ia
    graphics-set-origin graphics-set-origin-ia
    graphics-set-extents-ia
    graphics-set-mode graphics-set-color graphics-set-line-width
    ;; call-backs
    graphics-move-point graphics-insert-point
    graphics-remove-point graphics-last-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequently used subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree-at p)
  (tree->stree (subtree (the-buffer) p)))

(define (graphics-graphics-path)
  ;; path to innermost graphics tag
  (let* ((p (cDr (tm-where)))
	 (t (stree-at p)))
    (if (func? t 'graphics) p
	(with q (search-upwards "graphics")
	  (if (null? q) #f q)))))

(define (graphics-active-path)
  ;; path to active tag
  (with p (cDr (tm-where))
    (if (in? (car (stree-at p)) '(point line cline spline cspline)) p #f)))

(define (graphics-group-path)
  ;; path to innermost group
  (graphics-graphics-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global geometry of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-graphics)
  (insert-go-to
   '(with "gr-mode" "point"
          "gr-frame" (tuple "scale" "1cm" (tuple "0.5par" "0cm"))
	  "gr-clip"  (tuple "clip"
			    (tuple "0par" "-0.3par")
			    (tuple "1par" "0.3par"))
      (graphics))
   '(6 1)))

(define (graphics-set-property var val)
  (with p (graphics-graphics-path)
    (if p (tm-insert-with p var (stree->tree val)))))

(define (graphics-remove-property var)
  (with p (graphics-graphics-path)
    (if p (tm-remove-with p var))))

(define (graphics-cartesian-frame)
  (with frame (tree->stree (get-env-tree "gr-frame"))
    (if (match? frame '(tuple "scale" :2))
	frame
	'(tuple "scale" "1cm" (tuple "0.5par" "0cm")))))

(define (graphics-set-unit u)
  (with frame (graphics-cartesian-frame)
    (with new-frame `(tuple "scale" ,u ,(cAr frame))
      (graphics-set-property "gr-frame" new-frame))))

(define (graphics-set-unit-ia)
  (interactive '("Graphical unit:") 'graphics-set-unit))

(define (graphics-set-origin x y)
  (with frame (graphics-cartesian-frame)
    (with new-frame (append (cDr frame) `((tuple ,x ,y)))
      (graphics-set-property "gr-frame" new-frame))))

(define (graphics-set-origin-ia)
  (interactive
    '("Origin's x-coordinate:" "Origin's y-coordinate:")
    'graphics-set-origin))

(define (graphics-set-extents-ia)
  (interactive
    '("Left corner:" "Bottom corner:" "Right corner:" "Top corner:")
    '(lambda (l b r t)
       (with clip `(tuple "clip" (tuple ,l ,b) (tuple ,r ,t))
	 (graphics-set-property "gr-clip" clip)))))

(define (graphics-set-mode val)
  (graphics-group-start)
  (graphics-set-property "gr-mode" val))

(define (graphics-set-color val)
  (graphics-set-property "gr-color" val))

(define (graphics-set-line-width val)
  (graphics-set-property "gr-line-width" val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enriching graphics with properties like color, line width, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-enrich-filter l)
  (if (null? l) l
      (let* ((head (car l))
	     (tail (graphics-enrich-filter (cdr l))))
	(if (== (cadr head) "default") tail
	    (cons* (car head) (cadr head) tail)))))

(define (graphics-enrich-sub t l)
  (with f (graphics-enrich-filter l)
    (if (null? f)
	t
	`(with ,@f ,t))))

(define (graphics-enrich t)
  (let* ((mode (graphics-mode))
	 (color (get-env "gr-color"))
	 (lw (get-env "gr-line-width")))
    (cond ((== mode 'point)
	   (graphics-enrich-sub t `(("color" , color))))
	  ((in? mode '(line cline spline cspline))
	   (graphics-enrich-sub t `(("color" , color) ("line-width" ,lw)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the innermost group of graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-group-insert t)
  (with p (graphics-group-path)
    (if p (with n (- (length (stree-at p)) 1)
	    (tm-insert (rcons p n) (stree->tree (list 'tuple t)))
	    (if (func? t 'with)
		(tm-go-to (append p (list n (- (length t) 2) 1)))
		(tm-go-to (append p (list n 1))))))))

(define (graphics-group-enrich-insert t)
  (graphics-group-insert (graphics-enrich t)))

(define (graphics-group-start)
  (graphics-finish)
  (with p (graphics-group-path)
    (if p (tm-go-to (rcons p 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for modifying the active tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-active-stree)
  (with p (graphics-active-path)
    (if p (tree->stree (subtree (the-buffer) p)) #f)))

(define (graphics-active-type)
  (with t (graphics-active-stree)
    (if t (car t) #f)))

(define (graphics-active-assign t)
  (with p (graphics-active-path)
    (if p (begin
	    (tm-assign p (stree->tree t))
	    (tm-go-to (rcons p 1))))))

(define (graphics-active-set-tag l)
  (with t (graphics-active-stree)
    (if t (graphics-active-assign (cons l (cdr t))))))

(define (graphics-active-insert t)
  (with p (graphics-active-path)
    (if p (with n (- (length (stree-at p)) 1)
	    (tm-insert (rcons p n) (stree->tree (list 'tuple t)))
	    (tm-go-to (rcons p 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-mode)
  (string->symbol (get-env "gr-mode")))

(define (graphics-insert-point x y)
  ;(display* "Graphics] Insert " x ", " y "\n")
  (with mode (graphics-mode)
    (cond ((== mode 'point)
	   (graphics-group-enrich-insert `(point ,x ,y)))
	  ((in? mode '(line cline))
	   (if (== (graphics-active-type) 'line)
	       (graphics-active-insert `(point ,x ,y))
	       (graphics-group-enrich-insert `(line (point ,x ,y)))))
	  ((in? mode '(spline cspline))
	   (if (in? (graphics-active-type) '(spline cspline))
	       (graphics-active-insert `(point ,x ,y))
	       (graphics-group-enrich-insert `(,mode (point ,x ,y)))))
	  (else (display* "Uncaptured insert " x ", " y "\n")))))

(define (graphics-remove-point x y)
  ;(display* "Graphics] Remove " x ", " y "\n")
  (with mode (graphics-mode)
    (cond (else (display* "Uncaptured remove " x ", " y "\n")))))

(define (graphics-last-point x y)
  ;(display* "Graphics] Last " x ", " y "\n")
  (with mode (graphics-mode)
    (cond ((== mode 'point)
	   (graphics-group-enrich-insert `(point ,x ,y)))
	  ((in? mode '(line cline))
	   (graphics-active-insert `(point ,x ,y))
	   (if (== mode 'cline) (graphics-active-set-tag 'cline))
	   (graphics-group-start))
	  ((in? mode '(spline cspline))
	   (graphics-active-insert `(point ,x ,y))
	   (graphics-group-start))
	  (else (display* "Uncaptured last " x ", " y "\n")))))

(define (graphics-finish)
  ;(display* "Graphics] Finish\n")
  (with mode (graphics-mode)
    (cond ((== mode 'point) (noop))
	  ((in? mode '(line cline spline cspline)) (noop))
	  (else (display* "Uncaptured finish\n")))))
