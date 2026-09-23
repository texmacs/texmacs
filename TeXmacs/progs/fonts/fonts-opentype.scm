;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-opentype.scm
;; DESCRIPTION : profiles of OpenType math fonts and their text companions
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-opentype))

;; A profile records what the MATH table of a font cannot tell: the text
;; companions of the same design, whether math letters should come from
;; the math font itself or from the text italic, a bold math font, the
;; menu label. Family names are those of the TeXmacs font database. See
;; doc/opentype-math-fonts-survey.md.
;;
;; Keys: text, sans, mono (companion families), file (file name of the
;; math font without suffix, to test for its presence), letters (math or
;; text), bold-math (family of a bold math font), menu (label), group.
;;
;; A companion is named the way the `font' environment variable names one,
;; that is by its MASTER, the second field of an entry of
;; TeXmacs/fonts/font-features.scm, not by the family. The master of
;; `Fira Sans' is `Fira' and the master of `KpRoman' is `Kepler'; naming
;; the family instead makes the font selection fall back to the feature
;; distance and print "missing 'Fira Sans' master". The variant (roman,
;; sans serif, typewriter) picks the family inside the master, which is
;; why the three keys often repeat the same name.
;;
;; When two math fonts name the same text companion (Asana Math and TeX Gyre
;; Pagella Math are both Palladio designs), the first profile below is the
;; one that companion pulls in for formulas, so canonical pairings come
;; first.

(define-public-macro (define-math-font-profile name . props)
  `(math-font-profile-set ,name ',props))

(define-math-font-profile "Latin Modern Math"
  (file "latinmodern-math") (text "Latin Modern Roman")
  (sans "Latin Modern Sans") (mono "Latin Modern Mono")
  (letters "math") (menu "Latin Modern") (group "OpenType math"))

(define-math-font-profile "NewComputerModernMath"
  (file "NewCMMath-Regular") (text "NewComputerModern10")
  (sans "NewComputerModernSans10") (mono "NewComputerModernMono10")
  (letters "math") (bold-math "NewComputerModernMath")
  (menu "New Computer Modern") (group "OpenType math"))

(define-math-font-profile "TeX Gyre Pagella Math"
  (file "texgyrepagella-math") (text "TeX Gyre Pagella")
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Pagella") (group "TeX Gyre"))

(define-math-font-profile "TeX Gyre Termes Math"
  (file "texgyretermes-math") (text "TeX Gyre Termes")
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Termes") (group "TeX Gyre"))

(define-math-font-profile "TeX Gyre Bonum Math"
  (file "texgyrebonum-math") (text "TeX Gyre Bonum")
  (sans "TeX Gyre Adventor") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Bonum") (group "TeX Gyre"))

(define-math-font-profile "TeX Gyre Schola Math"
  (file "texgyreschola-math") (text "TeX Gyre Schola")
  (sans "TeX Gyre Heros") (mono "TeX Gyre Cursor")
  (letters "text") (menu "Schola") (group "TeX Gyre"))

(define-math-font-profile "TeX Gyre DejaVu Math"
  (file "texgyredejavu-math") (text "DejaVu")
  (sans "DejaVu") (mono "DejaVu")
  (letters "math") (menu "DejaVu") (group "TeX Gyre"))

(define-math-font-profile "Stix Two Math"
  (file "STIXTwoMath-Regular") (text "Stix Two Text")
  (letters "math") (menu "STIX Two") (group "OpenType math"))

(define-math-font-profile "XITS Math"
  (file "XITSMath-Regular") (text "Xits")
  (letters "math") (bold-math "XITS Math")
  (menu "XITS") (group "OpenType math"))

(define-math-font-profile "Libertinus Math"
  (file "LibertinusMath-Regular") (text "Libertinus")
  (sans "Libertinus") (mono "Libertinus")
  (letters "math") (menu "Libertinus") (group "OpenType math"))

(define-math-font-profile "KpMath"
  (file "KpMath-Regular") (text "Kepler")
  (sans "Kepler") (mono "Kepler")
  (letters "math") (bold-math "Kepler Math")
  (menu "Kp Fonts") (group "OpenType math"))

(define-math-font-profile "Asana Math"
  (file "Asana-Math") (text "TeX Gyre Pagella")
  (letters "math") (menu "Asana") (group "OpenType math"))

(define-math-font-profile "Fira Math"
  (file "FiraMath-Regular") (text "Fira")
  (sans "Fira") (mono "Fira")
  (letters "math") (menu "Fira Math") (group "OpenType math"))

(define-math-font-profile "Erewhon Math"
  (file "Erewhon-Math") (text "Erewhon")
  (letters "math") (menu "Erewhon") (group "OpenType math"))

(define-math-font-profile "XCharter Math"
  (file "XCharter-Math") (text "XCharter")
  (letters "math") (menu "XCharter") (group "OpenType math"))

(define-math-font-profile "Concrete Math"
  (file "Concrete-Math") (text "Concrete Math")
  (letters "math") (menu "Concrete Math") (group "OpenType math"))

(define-math-font-profile "Euler Math"
  (file "Euler-Math") (text "Euler Math")
  (letters "math") (menu "Euler Math") (group "OpenType math"))

(define-math-font-profile "IBM Plex Math"
  (file "IBMPlexMath-Regular") (text "IBM Plex")
  (sans "IBM Plex") (mono "IBM Plex")
  (letters "math") (menu "IBM Plex") (group "OpenType math"))

(define-math-font-profile "Lete Sans Math"
  (file "LeteSansMath") (text "Lete Sans Math")
  (letters "math") (menu "Lete Sans Math") (group "OpenType math"))

(define-math-font-profile "GFS Neohellenic Math"
  (file "GFSNeohellenicMath") (text "GFS Neohellenic")
  (letters "math") (menu "GFS Neohellenic") (group "OpenType math"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus: the profiled math fonts which are installed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opentype-math-font-installed? name)
  (with file (math-font-profile-attr name "file")
    (and (!= file "") (font-exists-in-tt? file))))

(tm-define (opentype-math-font-list)
  (:synopsis "Installed profiled math fonts as (label math-family text-family)")
  (with l (list-filter (math-font-profile-families) opentype-math-font-installed?)
    (list-sort (map (lambda (name)
                      (list (math-font-profile-attr name "menu") name
                            (math-font-profile-attr name "text")))
                    l)
               (lambda (a b) (string<? (car a) (car b))))))

(tm-menu (opentype-math-font-menu)
  (for (p (opentype-math-font-list))
    ((eval (car p)) (init-env "math-font" (cadr p)))))

(tm-menu (opentype-font-menu)
  (for (p (opentype-math-font-list))
    ((eval (car p)) (init-font (caddr p) (cadr p)))))
