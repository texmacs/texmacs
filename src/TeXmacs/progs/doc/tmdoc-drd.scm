
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-kbd.scm
;; DESCRIPTION : keyboard shortcuts for documentation
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-drd))

;; General groups

(define-group variant-tag
  (fragment-tag))

(define-group similar-tag
  (fragment-tag))

;; Special markup categories

(define-group fragment-tag
  framed-fragment scheme-fragment shell-fragment cpp-fragment mmx-fragment)

;; override variant function for verbatim and scm
;; FIXME: find a nicer solution

(tm-define (variant-circulate forward?)
  (:mode in-tmdoc?)
  (:inside verbatim)
  (with-innermost t 'verbatim
    (tree-assign-node! t 'scm)))

(tm-define (variant-circulate forward?)
  (:mode in-tmdoc?)
  (:inside scm)
  (with-innermost t 'scm
    (tree-assign-node! t 'verbatim)))
