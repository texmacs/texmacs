
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-drd.scm
;; DESCRIPTION : properties of the graphical tags
;; COPYRIGHT   : (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-drd)
  (:use (utils library cursor)
        (utils library tree)
        (utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of the graphical tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group graphical-atomic-tag
  point)

(define-group graphical-curve-tag
  line cline spline cspline arc carc)

(define-group graphical-text-tag
  text-at)

(define-group graphical-group-tag
  gr-group)

(define-group graphical-tag
  (graphical-curve-tag) (graphical-atomic-tag)
  (graphical-text-tag) (graphical-group-tag))

(tm-define gr-tags-all          (graphical-tag-list))
(tm-define gr-tags-curves       (graphical-curve-tag-list))
(tm-define gr-tags-noncurves    (append (graphical-atomic-tag-list)
                                        (graphical-text-tag-list)
                                        (graphical-group-tag-list)))
(tm-define gr-tags-oneshot      '(point arc carc text-at gr-group))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of graphical attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gr-prefixed? attr)
  (string-starts? attr "gr-"))

(tm-define (gr-prefix attr)
  (string-append "gr-" attr))

(tm-define (gr-unprefix attr)
  (substring attr 3 (string-length attr)))

(define-table attribute-default-table
  ("gid" . "default")
  ("magnification" . "1")
  ("color" . "black")
  ("opacity" . "100%")
  ("point-style" . "disk")
  ("line-width" . "1ln")
  ("dash-style" . "none")
  ("dash-style-unit" . "5ln")
  ("line-arrows" . "none")
  ("line-caps" . "normal")
  ("fill-color" . "none")
  ("fill-style" . "plain")
  ("text-at-halign" . "left")
  ("text-at-valign" . "base"))

(tm-define (graphics-attribute-default attr)
  (if (gr-prefixed? attr)
      (graphics-attribute-default (gr-unprefix attr))
      (ahash-ref attribute-default-table attr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes of the graphical tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-common-attributes)
  '("gid" "magnification" "color" "opacity"))

(tm-define (graphics-all-attributes)
  (map car (ahash-table->list attribute-default-table)))

(tm-define (graphics-attributes tag)
  (graphics-common-attributes))

(tm-define (graphics-attributes tag)
  (:require (== tag 'point))
  (append (graphics-common-attributes)
          '("fill-color"
            "point-style")))

(tm-define (graphics-attributes tag)
  (:require (graphical-curve-tag? tag))
  (append (graphics-common-attributes)
          '("fill-color"
            "line-width" "dash-style" "dash-style-unit" "line-arrows")))
  
(tm-define (graphics-attributes tag)
  (:require (graphical-text-tag? tag))
  (append (graphics-common-attributes)
          '("text-at-halign" "text-at-valign")))

(tm-define (graphics-attributes tag)
  (:require (graphical-group-tag? tag))
  (graphics-all-attributes))

(tm-define (graphics-attribute? tag attr)
  (in? attr (graphics-attributes tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes for editing modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-mode-attributes mode)
  (cond ((func? mode 'edit 1) (graphics-attributes (cadr mode)))
        ((== mode '(group-edit props)) (graphics-all-attributes))
        (else '())))

(tm-define (graphics-mode-attribute? mode attr)
  (in? attr (graphics-mode-attributes mode)))
