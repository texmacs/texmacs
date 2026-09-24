
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-short-menu.scm
;; DESCRIPTION : the text fonts which the focus bar offers for quick selection
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-short-menu)
  (:use (fonts fonts-opentype)))

;; The fonts of this table carry no mathematics of their own: choosing one
;; changes the text of the document and leaves formulas to the mathematical
;; font.  The fonts which do bring mathematics along are those profiled in
;; fonts-opentype.scm, and the menu lists them apart.
;;
;; A font is named by its MASTER, the second field of an entry of
;; TeXmacs/fonts/font-features.scm, since that is what the `font' environment
;; variable holds; `Fira Sans' belongs to the master `Fira' and `KpRoman' to
;; `Kepler'.  The keys are: menu (label, the name by default), family (a
;; family of the master, to ask the font database whether it is installed,
;; the name by default), file (a font file to look for instead, for the
;; fonts which TeXmacs ships and the database may ignore) and kind (serif,
;; sans, mono or other, which decides the submenu).

(define text-font-table (make-ahash-table))
(define text-font-names (list))

(tm-define (text-font-declare name props)
  (when (not (ahash-ref text-font-table name))
    (set! text-font-names (rcons text-font-names name)))
  (ahash-set! text-font-table name props))

(define-public-macro (define-text-font name . props)
  `(text-font-declare ,name ',props))

(define (text-font-attr name key default)
  (with p (assq key (or (ahash-ref text-font-table name) (list)))
    (if (and p (list-2? p)) (cadr p) default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-text-font "Alegreya")
(define-text-font "Baskerville")
(define-text-font "Cardo")
(define-text-font "Charter")
(define-text-font "Cochin")
(define-text-font "Crimson")
(define-text-font "DejaVu" (family "DejaVu Serif"))
(define-text-font "Didot")
(define-text-font "EB Garamond")
(define-text-font "Erewhon")
(define-text-font "Essays1743" (file "Essays1743"))
(define-text-font "Garamond")
(define-text-font "Georgia")
(define-text-font "Hoefler Text")
(define-text-font "IBM Plex" (family "IBM Plex Serif"))
(define-text-font "Iowan Old Style")
(define-text-font "Junicode")
(define-text-font "Kepler" (family "KpRoman") (menu "Kp Fonts"))
(define-text-font "Latin Modern Roman" (menu "Latin Modern"))
(define-text-font "Liberation" (family "Liberation Serif"))
(define-text-font "Libertinus" (family "Libertinus Serif"))
(define-text-font "Linux Libertine")
(define-text-font "Merriweather")
(define-text-font "NewComputerModern10" (menu "New Computer Modern"))
(define-text-font "Noto" (family "Noto Serif") (menu "Noto Serif"))
(define-text-font "Old Standard")
(define-text-font "Palatino")
(define-text-font "PT" (family "PT Serif") (menu "PT Serif"))
(define-text-font "Stix Two Text" (menu "STIX Two"))
(define-text-font "TeX Gyre Bonum" (menu "Bonum"))
(define-text-font "TeX Gyre Pagella" (menu "Pagella"))
(define-text-font "TeX Gyre Schola" (menu "Schola"))
(define-text-font "TeX Gyre Termes" (menu "Termes"))
(define-text-font "Times New Roman")
(define-text-font "Xits" (menu "XITS"))
(define-text-font "XCharter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sans serif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-text-font "Arial" (kind sans))
(define-text-font "Avenir" (kind sans))
(define-text-font "Cuprum" (kind sans))
(define-text-font "Fira" (family "Fira Sans") (kind sans))
(define-text-font "Futura" (kind sans))
(define-text-font "Gill" (family "Gill Sans") (menu "Gill Sans") (kind sans))
(define-text-font "Helvetica" (kind sans))
(define-text-font "Inter" (kind sans))
(define-text-font "Lato" (kind sans))
(define-text-font "Linux Biolinum" (kind sans))
(define-text-font "Lucida Grande" (kind sans))
(define-text-font "Open" (family "Open Sans") (menu "Open Sans") (kind sans))
(define-text-font "Optima" (kind sans))
(define-text-font "Roboto" (kind sans))
(define-text-font "Source" (family "Source Sans") (menu "Source Sans")
  (kind sans))
(define-text-font "TeX Gyre Adventor" (menu "Adventor") (kind sans))
(define-text-font "TeX Gyre Heros" (menu "Heros") (kind sans))
(define-text-font "Verdana" (kind sans))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typewriter and the rest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-text-font "Andale" (family "Andale Mono") (menu "Andale Mono")
  (kind mono))
(define-text-font "Courier New" (kind mono))
(define-text-font "Menlo" (kind mono))
(define-text-font "Monaco" (kind mono))
(define-text-font "TeX Gyre Cursor" (menu "Cursor") (kind mono))

(define-text-font "American Typewriter" (kind other))
(define-text-font "Chalkboard" (kind other))
(define-text-font "Chalkduster" (kind other))
(define-text-font "Marker Felt" (kind other))
(define-text-font "Meyne Textur" (file "meyne_textur") (kind other))
(define-text-font "Papyrus" (kind other))
(define-text-font "TeX Gyre Chorus" (menu "Chorus") (kind other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The menus, which only show what is installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (text-font-installed? name)
  (with file (text-font-attr name 'file "")
    (if (!= file "")
        (font-exists-in-tt? file)
        (nnull? (font-database-styles (text-font-attr name 'family name))))))

(tm-define (text-font-list kind)
  (:synopsis "Installed text fonts of a given @kind, as (label master) pairs")
  ;; a font which an installed OpenType math font brings along is left out:
  ;; the menu offers it above, in the company of its mathematics
  (let* ((skip (opentype-math-companions))
         (keep? (lambda (name)
                  (and (== (text-font-attr name 'kind 'serif) kind)
                       (not (in? name skip))
                       (text-font-installed? name))))
         (l (list-filter text-font-names keep?))
         (labeled (map (lambda (name)
                         (list (text-font-attr name 'menu name) name))
                       l)))
    (list-sort labeled
               (lambda (a b) (string<=? (locase-all (car a))
                                        (locase-all (car b)))))))

(tm-menu (text-font-kind-menu kind)
  (for (p (text-font-list kind))
    ((eval (car p)) (init-font (cadr p)))))

(tm-menu (document-short-text-font-menu)
  (assuming (nnull? (text-font-list 'serif))
    (-> "Serif" (dynamic (text-font-kind-menu 'serif))))
  (assuming (nnull? (text-font-list 'sans))
    (-> "Sans serif" (dynamic (text-font-kind-menu 'sans))))
  (assuming (nnull? (text-font-list 'mono))
    (-> "Typewriter" (dynamic (text-font-kind-menu 'mono))))
  (assuming (nnull? (text-font-list 'other))
    (-> "Decorative" (dynamic (text-font-kind-menu 'other)))))
