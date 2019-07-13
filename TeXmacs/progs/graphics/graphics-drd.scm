
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

(define-group variant-tag
  (graphical-over-under-tag))

(define-group graphical-atomic-tag
  point)

(define-group graphical-open-curve-tag
  line spline bezier smooth arc)

(define-group graphical-closed-curve-tag
  cline cspline cbezier csmooth carc)

(define-group graphical-curve-tag
  (graphical-open-curve-tag) (graphical-closed-curve-tag))

(define-group graphical-short-text-tag
  text-at math-at)

(define-group graphical-long-text-tag
  document-at)

(define-group graphical-text-tag
  (graphical-short-text-tag) (graphical-long-text-tag))

(define-group graphical-contains-curve-tag
  (graphical-curve-tag))

(define-group graphical-contains-text-tag
  (graphical-text-tag))

(define-group graphical-non-group-tag
  (graphical-curve-tag) (graphical-atomic-tag) (graphical-text-tag))

(define-group graphical-group-tag
  gr-group)

(define-group graphical-tag
  (graphical-non-group-tag) (graphical-group-tag))

(define-group graphical-over-under-tag
  draw-over draw-under)

(tm-define (graphical-context? t)
  (tm-in? t (graphical-tag-list)))

(tm-define (graphical-text-context? t)
  (tm-in? t (graphical-text-tag-list)))

(tm-define (graphical-long-text-context? t)
  (tm-in? t (graphical-long-text-tag-list)))

(tm-define (graphical-text-at-context? t)
  (and (graphical-text-context? t) (>= (tm-arity t) 2)))

(tm-define (graphical-long-text-at-context? t)
  (and (graphical-long-text-context? t) (>= (tm-arity t) 2)))

(tm-define (graphical-text-arg-context? t)
  (and (graphical-text-context? t) (< (tm-arity t) 2)))

(tm-define (graphical-over-under-context? t)
  (tm-in? t (graphical-over-under-tag-list)))

(tm-define (inside-graphical-text?)
  (tree-innermost graphical-text-context?))

(tm-define (inside-graphical-over-under?)
  (tree-innermost graphical-over-under-context?))

(tm-define gr-tags-user      (list))
(tm-define gr-tags-all       (graphical-tag-list))
(tm-define gr-tags-curves    (graphical-curve-tag-list))
(tm-define gr-tags-noncurves (append (graphical-atomic-tag-list)
                                     (graphical-text-tag-list)
                                     (graphical-group-tag-list)))

(tm-define (graphical-user-tag? l)
  (in? l gr-tags-user))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of graphical attributes and their properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gr-prefixed? attr)
  (string-starts? attr "gr-"))

(tm-define (gr-prefix attr)
  (string-append "gr-" attr))

(tm-define (gr-unprefix attr)
  (substring attr 3 (string-length attr)))

(define-table attribute-default-table
  ("gid" . "default")
  ("anim-id" . "default")
  ("proviso" . "true")
  ("magnify" . "1")
  ("color" . "black")
  ("opacity" . "100%")
  ("point-style" . "disk")
  ("point-size" . "2.5ln")
  ("point-border" . "1ln")
  ("line-width" . "1ln")
  ("line-join" . "normal")
  ("line-caps" . "normal")
  ("line-effects" . "normal")
  ("line-portion" . "1")
  ("dash-style" . "none")
  ("dash-style-unit" . "5ln")
  ("arrow-begin" . "none")
  ("arrow-end" . "none")
  ("arrow-length" . "5ln")
  ("arrow-height" . "5ln")
  ("fill-color" . "none")
  ("fill-style" . "plain")
  ("text-at-halign" . "left")
  ("text-at-valign" . "base")
  ("text-at-margin" . "1spc")
  ("doc-at-valign" . "top")
  ("doc-at-width" . "1par")
  ("doc-at-hmode" . "min")
  ("doc-at-ppsep" . "0fn")
  ("doc-at-border" . "0ln")
  ("doc-at-padding" . "0spc"))

(tm-define (graphics-attribute-default attr)
  (if (gr-prefixed? attr)
      (graphics-attribute-default (gr-unprefix attr))
      (ahash-ref attribute-default-table attr)))

(tm-define (decode-dash x)
  (cond ((== x "default") "---")
        ((== x "10") ". . . . .")
        ((== x "11100") "- - - - -")
        ((== x "1111010") "- . - . -")
        ((string-alpha? x) x)
        (else "other")))

(tm-define (decode-arrow x)
  (cond ((== x "default") "")
        ((== x "none") "")
        ((== x "") "")
        ((== x "<gtr>") ">")
        ((== x "|<gtr>") "|>")
        ((== x "<gtr><gtr>") ">>")
        ((== x "<less>") "<")
        ((== x "<less>|") "<|")
        ((== x "<less><less>") "<<")
        ((string? x) x)
        (else "other")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes of the graphical tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-common-attributes)
  '("gid" "anim-id" "proviso" "magnify" "color" "opacity"))

(tm-define (graphics-all-attributes)
  (map car (ahash-table->list attribute-default-table)))

(tm-define (graphics-attributes tag)
  (graphics-common-attributes))

(tm-define (graphics-attributes tag)
  (:require (== tag 'point))
  (append (graphics-common-attributes)
          '("fill-color" "point-style" "point-size" "point-border")))

(tm-define (graphics-attributes tag)
  (:require (or (graphical-curve-tag? tag) (graphical-user-tag? tag)))
  (append (graphics-common-attributes)
          '("fill-color"
            "line-width" "line-join" "line-caps" "line-effects" "line-portion"
            "dash-style" "dash-style-unit"
            "arrow-begin" "arrow-end" "arrow-length" "arrow-height")))

(tm-define (graphics-attributes tag)
  (:require (graphical-text-tag? tag))
  (append (graphics-common-attributes)
          '("text-at-halign" "text-at-valign" "text-at-margin")))

(tm-define (graphics-attributes tag)
  (:require (graphical-long-text-tag? tag))
  (append (graphics-common-attributes)
          '("text-at-halign" "doc-at-valign" "text-at-margin"
            "fill-color" "doc-at-width" "doc-at-hmode"
            "doc-at-ppsep" "doc-at-border" "doc-at-padding")))

(tm-define (graphics-attributes tag)
  (:require (graphical-group-tag? tag))
  (graphics-all-attributes))

(tm-define (graphics-attribute? tag attr)
  (in? attr (graphics-attributes tag)))

(tm-define (graphics-valign-var t)
  (cond ((in? t (graphical-short-text-tag-list)) "text-at-valign")
        ((in? t (graphical-long-text-tag-list)) "doc-at-valign")
        ((tree? t) (graphics-valign-var (tree-label t)))
        ((pair? t) (graphics-valign-var (car t)))
        (else "text-at-valign")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes for editing modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-mode-attributes mode)
  (cond ((func? mode 'edit 1) (graphics-attributes (cadr mode)))
        ((func? mode 'hand-edit 1) (graphics-attributes (cadr mode)))
        ((== mode '(group-edit props)) (graphics-all-attributes))
        ((== mode '(group-edit edit-props)) (graphics-all-attributes))
        (else '())))

(tm-define (graphics-mode-attribute? mode attr)
  (in? attr (graphics-mode-attributes mode)))

(tm-define (graphics-get-anim-type) #f)

(tm-define (graphics-test-anim-type? val)
  (== (graphics-get-anim-type) val))

(tm-define (graphics-set-anim-type val)
  (:check-mark "*" graphics-test-anim-type?)
  (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refined properties concerning arity and types of children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-minimal? obj)
  (or (tm-in? obj '(point text-at math-at document-at))
      (== (tm-arity obj) (tag-minimal-arity (tm-car obj)))))

(tm-define (graphics-incomplete? obj)
  (< (tm-arity obj) (tag-minimal-arity (tm-car obj))))

(tm-define (graphics-complete? obj)
  (>= (tm-arity obj) (tag-maximal-arity (tm-car obj))))

(tm-define (graphics-complete obj)
  (list obj #f))
