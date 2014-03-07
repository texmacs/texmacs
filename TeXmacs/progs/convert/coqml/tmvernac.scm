;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmvernac.scm
;; DESCRIPTION : TeXmacs to Coq (vernacular) converter
;; COPYRIGHT   : (C) 2014  François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml tmvernac))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmvernac-string x)
  ;; should take care of charset.
  x)

(define (tmvernac-document l)
  `(!document ,@(map tmvernac l)))

(define (tmvernac-concat l)
  `(!concat ,@(map tmvernac l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DoCoq macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmvernac-coq-comment l)
  (tmvernac (car l)))

(define (tmvernac-coq-command l)
  (tmvernac (caddr l)))

(define (tmvernac-coq-enunciation l)
  (let ((kind  (tmvernac (caddr l)))
        (name  (tmvernac (cadddr l)))
        (body  (tmvernac (fifth l)))
        (proof (tmvernac (sixth l))))
  `(!paragraph
     (!concat ,kind " " ,name " : " ,body)
     "Proof."
     ,proof
     "Qed.")))

(define (tmvernac-coq-section l)
  (let ((name (tmvernac (car l)))
        (body (tmvernac (cadr l))))
    `(!paragraph
       (!concat "Section" ,name ".")
       ,body
       (!concat "End"     ,name "."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs style macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmvernac-indent l)
  `(!indent ,(tmvernac (car l))))

;; Nota:
;; - we should add support for all TeXmacs primitives;
;; - we should also add support for all TeXmacs docoq primitives and useful
;;   style macro;
;; - we should expand all other things.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-dispatcher tmvernac-methods%
  ; TeXmacs primitives
  (document        tmvernac-document)
  (concat          tmvernac-concat)

  ; DoCoq macros
  (coq-comment     tmvernac-coq-comment)
  (coq-command     tmvernac-coq-command)
  (coq-enunciation tmvernac-coq-enunciation)
  (coq-section     tmvernac-coq-section)

  ; TeXmacs style macros
  (indent          tmvernac-indent))

(define (tmvernac-apply key args)
  (let ((n (length args))
        (r (logic-ref tmvernac-methods% key)))
    (if (not r ) (map write (list "tmvernac-apply: " key ", " args)))
    (if r (r args))))

(tm-define (tmvernac x)
  (cond ((string? x) (tmvernac-string x))
        ((list>0? x) (tmvernac-apply (car x) (cdr x)))
        (else "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->vernac x)
  (if (tree? x) (set! x (tree->stree x)))
  (display* x "\n\n")
  (with y (tmvernac x)
    (display* "\n\nresult: " y "\n\n")
    y))

(tm-define (texmacs->vernac-document x)
  (if (tree? x) (set! x (tree->stree x)))
  (if (tmfile? x)
    (with body (tmfile-extract x 'body)
      (texmacs->vernac body))
    (texmacs->vernac x)))
