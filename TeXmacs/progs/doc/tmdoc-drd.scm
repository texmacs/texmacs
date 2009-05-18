
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-kbd.scm
;; DESCRIPTION : keyboard shortcuts for documentation
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  framed-fragment shell-fragment scm-fragment cpp-fragment mmx-fragment)

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

(tm-define (variant-circulate forward?)
  (:mode in-mmxdoc?)
  (:inside verbatim)
  (with-innermost t 'verbatim
    (tree-assign-node! t 'mmx)))

(tm-define (variant-circulate forward?)
  (:mode in-mmxdoc?)
  (:inside mmx)
  (with-innermost t 'mmx
    (tree-assign-node! t 'cpp)))

(tm-define (variant-circulate forward?)
  (:mode in-mmxdoc?)
  (:inside cpp)
  (with-innermost t 'cpp
    (tree-assign-node! t 'verbatim)))
