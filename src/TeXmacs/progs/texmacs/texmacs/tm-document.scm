
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-document.scm
;; DESCRIPTION : setting global document properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-document)
  (:use (texmacs library length))
  (:export
    ;; preamble mode
    toggle-preamble
    ;; text and paragraph properties
    init-text-width init-font-size init-dpi init-first-indent
    init-interline init-interline-spc init-interpar-spc
    init-magn init-language init-color init-bg-color
    ;; page layout
    toggle-visible-header-and-footer
    init-page-margins init-screen-reduction init-page-size init-as-on-paper
    init-page-shrink init-page-extend init-page-flexibility))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preamble mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-preamble)
  (:synopsis "Toggle preamble mode.")
  (:check-mark "v" in-preamble?)
  (let ((new (if (string=? (get-env "preamble") "true") "false" "true")))
    (init-env "preamble" new)
    (if (== new "false")
	(begin (generate-all-aux) (update-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (init-text-width s) (init-env "paragraph width" s))
(define (init-font-size s) (init-env "font base size" s))
(define (init-dpi s) (init-env "dpi" s))
(define (init-first-indent s) (init-env "first indentation" s))
(define (init-interline s) (init-env "interline space" s))
(define (init-interline-spc s) (init-env "line stretch" s))
(define (init-interpar-spc s) (init-env "interparagraph space" s))
(define (init-magn s) (init-env "magnification" s))
(define (init-language lan)
  (let ((before (in? (tree->object (get-init-tree "language"))
		     '("russian" "ukrainian")))
	(after (in? lan '("russian" "ukrainian"))))
    (if (and before (not after)) (init-default "font"))
    (init-env "language" lan)
    (if (and after (not before)) (init-env "font" "cyrillic"))))
(define (init-color s) (init-env "color" s))
(define (init-bg-color s) (init-env "background color" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-page-medium? s) (string=? (get-env "page medium") s))
(set-check-mark! set-page-medium "*" test-page-medium?)

(define (test-page-type? s) (string=? (get-env "page type") s))
(set-check-mark! set-page-type "*" test-page-type?)

(define (test-page-orientation? s) (string=? (get-env "page orientation") s))
(set-check-mark! set-page-orientation "*" test-page-orientation?)

(define (visible-header-and-footer?)
  (== "true" (get-env "show header and footer")))

(tm-define (toggle-visible-header-and-footer)
  (:synopsis "Toggle visibility of headers and footers in 'page' paper mode.")
  (:check-mark "v" visible-header-and-footer?)
  (init-env "show header and footer"
	    (if (== (get-env "show header and footer") "true")
		"false" "true")))

(define (init-page-margins l r t b)
  (init-env "odd page margin" l)
  (init-env "even page margin" l)
  (init-env "page right margin" r)
  (init-env "page top margin" t)
  (init-env "page bottom margin" b)
  (init-text-width (length- (get-env "page width") l r)))

(define (init-screen-reduction l r t b)
  (init-env "reduction page left margin" l)
  (init-env "reduction page right margin" r)
  (init-env "reduction page top margin" t)
  (init-env "reduction page bottom margin" b))

(define (init-page-size w h)
  (init-env "page type" "user")
  (init-env "page width" w)
  (init-env "page height" h)
  (init-page-margins "5mm" "5mm" "5mm" "5mm")
  (init-screen-reduction "0cm" "0cm" "0cm" "0cm"))

(define (as-on-paper?)
  (and (visible-header-and-footer?)
       (length-zero? (get-env "reduction page left margin"))
       (length-zero? (get-env "reduction page right margin"))
       (length-zero? (get-env "reduction page top margin"))
       (length-zero? (get-env "reduction page bottom margin"))))

(tm-define (init-as-on-paper)
  (:synopsis "Let the screen margins be as on the real paper output.")
  (:check-mark "o" as-on-paper?)
  (init-screen-reduction "0mm" "0mm" "0mm" "0mm")
  (init-env "show header and footer" "true"))

(define (init-page-shrink s) (init-env "page shrink" s))
(define (init-page-extend s) (init-env "page extend" s))
(define (init-page-flexibility s) (init-env "page flexibility" s))
