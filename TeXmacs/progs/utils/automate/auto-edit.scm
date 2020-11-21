
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : auto-edit.scm
;; DESCRIPTION : Editing markup for automated document generation
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils automate auto-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DRD properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group implied-tag
  implied-generic implied-verbatim implied-scm implied-cpp implied-mmx
  implied-python implied-scilab implied-shell)

(define-group variant-tag
  (implied-tag))

(define-group similar-tag
  (implied-tag))

(define-group auto-block-tag
  block-if block-for block-while
  block-assign
  block-intersperse)

(define-group auto-inline-tag
  inline-if inline-for inline-while
  inline-assign
  inline-intersperse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting automated tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-auto-arg kind)
  (cond ((== kind :block) '(document ""))
        ((== kind :inline) "")
        ((== kind :code) '(implied-scm ""))
        (else "")))

(tm-define (make-block* lab . args)
  (with l (map make-auto-arg args)
    (with t `(document (,lab ,@l))
      (insert-go-to t (cons* 0 0 (path-start (car l) '()))))))

(tm-define (make-inline* lab . args)
  (with l (map make-auto-arg args)
    (with t `(,lab ,@l)
      (insert-go-to t (cons 0 (path-start (car l) '()))))))

(tm-define (make-block-if) (make-block* 'block-if :code :block))
(tm-define (make-block-if-else)
  (make-block* 'block-if-else :code :block :block))
(tm-define (make-block-for) (make-block* 'block-for :code :code :block))
(tm-define (make-block-while) (make-block* 'block-while :code :block))
(tm-define (make-block-tag) (make-block* 'block-texmacs-tag :inline :block))
(tm-define (make-block-assign) (make-block* 'block-assign :code :code))
(tm-define (make-block-intersperse)
  (make-block* 'block-intersperse :inline :block))

(tm-define (make-inline-if) (make-inline* 'inline-if :code :inline))
(tm-define (make-inline-if-else)
  (make-inline* 'inline-if-else :code :inline :inline))
(tm-define (make-inline-for) (make-inline* 'inline-for :code :code :inline))
(tm-define (make-inline-while) (make-inline* 'inline-while :code :inline))
(tm-define (make-inline-tag) (make-inline* 'inline-texmacs-tag :inline :inline))
(tm-define (make-inline-assign) (make-inline* 'inline-assign :code :code))
(tm-define (make-inline-intersperse)
  (make-inline* 'inline-intersperse :inline :inline))

(tm-define (make-output-string) (make-inline* 'output-string :code))
(tm-define (make-inline-output) (make-inline* 'inline-output :code))
(tm-define (make-block-output) (make-inline* 'block-output :code))
