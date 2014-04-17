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

(texmacs-module (convert coqml tmvernac)
          (:use (convert coqml vernacout)))

(define mode "text")

(define (initialize-converter)
  (set! mode "text"))

(define-macro (with-mode new-mode x)
  `(let ((old-mode mode))
     (set! mode ,new-mode)
     (let ((r ,x))
       (set! mode old-mode)
       r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmvernac-string x)
  ;; should take care of escaping ($, #, %).
  (cork->sourcecode x))

(define (tmvernac-document s l)
  `(!document ,@(map tmvernac l)))

(define (tmvernac-concat s l)
  `(!concat ,@(map tmvernac l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DoCoq macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmvernac-coq-coqdoc s l)
  (with-mode "coqdoc"
    (tmvernac (car l))))
;    `(!document (!coqdoc ,(tmvernac (car l))))))

(define (tmvernac-coq-comment s l)
  (with-mode "code"
    (tmvernac (car l))))
;    `(!document (!comment ,(tmvernac (car l))))))

(define (tmvernac-coq-command s l)
  (with-mode "code"
    (tmvernac (caddr l))))

(define (tmvernac-coq-enunciation s l)
  (with-mode "code"
    (let ((kind  (tmvernac (caddr l)))
          (name  (tmvernac (cadddr l)))
          (body  (tmvernac (fifth l)))
          (proof (tmvernac (sixth l))))
      `(!paragraph
         (!concat ,kind " " ,name " " ,body)
         "Proof."
         ,proof))))

(define (tmvernac-coq-section s l)
  (with-mode "code"
    (let ((name (tmvernac (car l)))
          (body (tmvernac (cadr l))))
      `(!paragraph
         (!concat "Section" ,name ".")
         ,body
         (!concat "End"     ,name ".")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoqDoc macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: require mode= doc

(define (tmcoqdoc-sectionning s l)
  (let ((body (tmvernac (car l)))
        (mark (cond ((== s 'paragraph)     "**** ")
                    ((== s 'subsubsection) "*** ")
                    ((== s 'subsection)    "** ")
                    ((== s 'section)       "* ")
                    (else ""))))
    `(!concat ,mark ,body)))

(define (tmcoqdoc-folds s l)
  (let ((body (tmvernac (cadr l)))
        (mark (cond ((== s 'folded)     "hide")
                    ((== s 'unfolded)   "show")
                    (else ""))))
    `(!paragraph
       (!concat "(* begin " ,mark " *)")
       ,body
       (!concat "(* end " ,mark " *)"))))

;; NOTA: could be merged in a "tmcoqdoc-delimited" function
(define (tmcoqdoc-coq s l)
  (with coq (tmvernac (car l))
    `(!concat "[" ,coq "]")))

(define (tmcoqdoc-vernac s l)
  (with vernac (tmvernac (car l))
    `(!concat "[[\n" ,vernac "\n]]")))

(define (tmcoqdoc-latex s l)
  (with tex (tmvernac-string
              (texmacs->generic (stree->tree (car l)) "latex-snippet"))
    (if (func? (car l) 'math 1)
      `(!concat ,tex)
      `(!concat "%" ,tex "%"))))

(define (tmcoqdoc-html s l)
  (with html (tmvernac-string
               (texmacs->generic (stree->tree (car l)) "html-snippet"))
    `(!concat "#" ,html "#")))

(define (tmcoqdoc-verbatim s l)
  (with verb (tmvernac (car l))
    `(!concat "<<\n" ,verb "\n>>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs style macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: require mode= doc

(define (tmvernac-hrule s l) "----")

(define (tmvernac-item s l) '(!item "- "))

(define (tmvernac-itemize s l)
  `(!paragraph ,(tmvernac (car l))))

(define (tmvernac-indent s l)
  `(!paragraph (!indent ,(tmvernac (car l)))))

(define (tmvernac-emphasis s l)
  (with coq (tmvernac (car l))
    `(!concat "_" ,coq "_")))

;; Nota:
;; - we should add support for all TeXmacs primitives;
;; - we should also add support for all TeXmacs docoq primitives and useful
;;   style macro;
;; - we should expand all other things.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: Add arity verification.
(logic-dispatcher tmvernac-methods%
  ; TeXmacs primitives
  (document        tmvernac-document)
  (concat          tmvernac-concat)

  ; DoCoq macros
  (coq-command     tmvernac-coq-command)
  (coq-comment     tmvernac-coq-comment)
  (coq-coqdoc      tmvernac-coq-coqdoc)
  (coq-enunciation tmvernac-coq-enunciation)
  (coq-section     tmvernac-coq-section)
  ; CoqDoc
  (coqdoc-coq      tmcoqdoc-coq)
  (coqdoc-html     tmcoqdoc-html)
  (coqdoc-latex    tmcoqdoc-latex)
  (coqdoc-vernac   tmcoqdoc-vernac)
  (coqdoc-verbatim tmcoqdoc-verbatim)

  ; TeXmacs style macros
  ((:or section subsection subsubsection paragraph) tmcoqdoc-sectionning)
  ((:or folded unfolded) tmcoqdoc-folds)
  (em              tmvernac-emphasis)
  (itemize         tmvernac-itemize)
  (item            tmvernac-item)
  (hrule           tmvernac-hrule)
  (indent          tmvernac-indent))

(define (tmvernac-apply key args)
  (let ((n (length args))
        (r (logic-ref tmvernac-methods% key)))
    (if (not r )
      (begin (map write (list "tmvernac-apply: " key ", " args "\nr: " r))
             (newline)))
    (if r (r key args))))

(tm-define (tmvernac x)
  ;; TODO: catching errors and save/restore state in order to maximize the
  ;; robustness of the converter
  (cond ((string? x) (tmvernac-string x))
        ((list>0? x) (tmvernac-apply (car x) (cdr x)))
        (else "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->vernac x)
  (initialize-converter)
  (if (tree? x) (set! x (tree->stree x)))
  (display* x "\n\n")
  (let ((y (tmvernac x)))
    (display* "\n\nresult: " y "\n\n")
    (serialize-vernac y)))

(tm-define (texmacs->vernac-document x)
  (if (tree? x) (set! x (tree->stree x)))
  (if (tmfile? x)
    (with body (tmfile-extract x 'body)
      (texmacs->vernac body))
    (texmacs->vernac x)))
