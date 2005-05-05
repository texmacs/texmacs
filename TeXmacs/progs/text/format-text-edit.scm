
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-text-edit.scm
;; DESCRIPTION : routines for formatting text in text mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text format-text-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-line-with var val)
  (:synopsis "Make 'with' with one or more paragraphs as its scope")
  (:check-mark "o" test-env?)
  (if (not (selection-active-normal?))
      (select-line))
  (make-with var val)
  (insert-return)
  (remove-text #f))

(tm-define (set-left-margin s) (make-line-with "par-left" s))
(tm-define (set-right-margin s) (make-line-with "par-right" s))
(tm-define (set-first-indent s) (make-line-with "par-first" s))
(tm-define (set-last-indent s) (make-line-with "last-indentation" s))
(tm-define (set-interline s) (make-line-with "par-sep" s))
(tm-define (set-interline-spc s) (make-line-with "par-line-sep" s))
(tm-define (set-interpar-spc s) (make-line-with "par-par-sep" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-page-break)
  (make 'page-break)
  (insert-return))

(tm-define (make-new-page)
  (make 'new-page)
  (insert-return))

(tm-define (make-new-dpage)
  (make 'new-dpage)
  (insert-return))
