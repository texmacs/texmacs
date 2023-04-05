
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-latex.scm
;; DESCRIPTION : setup latex converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex init-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-recognizes-at? s pos)
  (set! pos (format-skip-spaces s pos))
  (cond ((format-test? s pos "\\document") #t)
        ((format-test? s pos "\\usepackage") #t)
        ((format-test? s pos "\\input") #t)
        ((format-test? s pos "\\includeonly") #t)
        ((format-test? s pos "\\chapter") #t)
        ((format-test? s pos "\\appendix") #t)
        ((format-test? s pos "\\section") #t)
        ((format-test? s pos "\\begin") #t)
        (else #f)))

(define (latex-recognizes? s)
  (and (string? s) (latex-recognizes-at? s 0)))

(define-format latex
  (:name "LaTeX")
  (:suffix "tex")
  (:recognize latex-recognizes?))

(define-format latex-class
  (:name "LaTeX class")
  (:suffix "ltx" "sty" "cls"))

(define-preferences
  ("texmacs->latex:transparent-tracking" "on" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs->LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (convert latex texout) serialize-latex)
(lazy-define (convert latex tmtex) texmacs->latex)

(converter texmacs-stree latex-stree
  (:function-with-options texmacs->latex)
  (:option "texmacs->latex:source-tracking" "off")
  (:option "texmacs->latex:conservative" "on")
  (:option "texmacs->latex:transparent-source-tracking" "on")
  (:option "texmacs->latex:attach-tracking-info" "on")
  (:option "texmacs->latex:replace-style" "on")
  (:option "texmacs->latex:expand-macros" "on")
  (:option "texmacs->latex:expand-user-macros" "off")
  (:option "texmacs->latex:indirect-bib" "off")
  (:option "texmacs->latex:use-macros" "on")
  (:option "texmacs->latex:encoding" "ascii"))

(converter latex-stree latex-document
  (:function serialize-latex))

(converter latex-stree latex-snippet
  (:function serialize-latex))

(tm-define (texmacs->latex-document x opts)
  (serialize-latex (texmacs->latex (tm->stree x) opts)))

(converter texmacs-stree latex-document
  (:function-with-options conservative-texmacs->latex)
  ;;(:function-with-options tracked-texmacs->latex)
  (:option "texmacs->latex:source-tracking" "off")
  (:option "texmacs->latex:conservative" "on")
  (:option "texmacs->latex:transparent-source-tracking" "on")
  (:option "texmacs->latex:attach-tracking-info" "on")
  (:option "texmacs->latex:replace-style" "on")
  (:option "texmacs->latex:expand-macros" "on")
  (:option "texmacs->latex:expand-user-macros" "off")
  (:option "texmacs->latex:indirect-bib" "off")
  (:option "texmacs->latex:use-macros" "on")
  (:option "texmacs->latex:encoding" "ascii"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX -> TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-document->texmacs x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (with as-pic (== (get-preference "latex->texmacs:fallback-on-pictures") "on")
    (conservative-latex->texmacs x as-pic)))

(converter latex-document latex-tree
  (:function parse-latex-document))

(converter latex-snippet latex-tree
  (:function parse-latex))

(converter latex-document texmacs-tree
  (:function-with-options latex-document->texmacs)
  (:option "latex->texmacs:fallback-on-pictures" "on")
  (:option "latex->texmacs:source-tracking" "off")
  (:option "latex->texmacs:conservative" "off")
  (:option "latex->texmacs:transparent-source-tracking" "off"))

(converter latex-class-document texmacs-tree
  (:function latex-class-document->texmacs))

(converter latex-tree texmacs-tree
  (:function latex->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (convert latex test-tmtex) test-tmtex)
