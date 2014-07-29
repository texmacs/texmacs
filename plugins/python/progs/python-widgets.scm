;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : python-widgets.scm
;; DESCRIPTION : Widgets for the python plugin
;; COPYRIGHT   : (C) 2014  Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (python-widgets)
  (:use (kernel gui menu-define) (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A very simple help widget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmpy-help-text   "No object selected")
(define tmpy-help-source "No source code available")
(define tmpy-help-file   "No source file available")

(define (tmpy-help-content)
  `(with "bg-color" "#fdfdfd"
     (document
       (with "ornament-shape" "rounded" "ornament-color" "pastel yellow"
             "par-left" "2em" "par-right" "2em"
             (ornamented
              (document
                (concat (htab "5mm") (large (strong "Description")) 
                        (htab "5mm")))))
       (with "par-left" "1em" "par-right" "1em" ; ignored, why?
             ,tmpy-help-text)
       (with "ornament-shape" "rounded" "ornament-color" "pastel yellow"
             "par-left" "2em" "par-right" "2em"
             (ornamented
              (document
                (concat (htab "5mm") (large (strong "Source code"))
                        (htab "5mm")))))
       (with "par-left" "1em" "par-right" "1em" ; ignored, why?
             (concat (htab "5mm") (em ,tmpy-help-file) (htab "5mm")))
       (with "par-left" "1em" "par-right" "1em" ; ignored, why?
             (python-code ,tmpy-help-source)))))

(tm-widget (tmpy-help-widget close)
  (resize ("400px" "800px" "4000px") ("300px" "600px" "4000px")
    (refreshable "tmpy-help-widget-content"
      (scrollable
        (texmacs-output 
         (stree->tree (tmpy-help-content))
         '(style "generic"))))
    (bottom-buttons >>> ("Close" (close)))))

(tm-define (tmpy-open-help text source file)
  (set! tmpy-help-text 
        (verbatim-snippet->texmacs text))
                                   ;(acons "verbatim->texmacs:wrap" "on" '())))
  (set! tmpy-help-source (verbatim-snippet->texmacs source))
  (set! tmpy-help-file file)
  (dialogue-window tmpy-help-widget noop "Python help"))

