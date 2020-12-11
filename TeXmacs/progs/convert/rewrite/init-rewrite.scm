
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-rewrite.scm
;; DESCRIPTION : setup texmacs converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite init-rewrite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main TeXmacs format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs-recognizes? s)
  (and (string? s)
       (or (string-starts? s "<TeXmacs")
           (string-starts? s "\\(\\)(TeXmacs")
           (string-starts? s "TeXmacs")
           (string-starts? s "edit"))))

(define-format texmacs
  (:name "TeXmacs")
  (:suffix "tm" "ts" "tp")
  (:must-recognize texmacs-recognizes?))

(converter texmacs-tree texmacs-stree
  (:function tree->stree))

(converter texmacs-stree texmacs-tree
  (:function stree->tree))

(converter texmacs-document texmacs-tree
  (:function parse-texmacs))

(converter texmacs-tree texmacs-document
  (:function serialize-texmacs))

(converter texmacs-snippet texmacs-tree
  (:function parse-texmacs-snippet))

(converter texmacs-tree texmacs-snippet
  (:function serialize-texmacs-snippet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme format for TeXmacs (no information loss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stm-recognizes? s)
  (and (string? s) (string-starts? s "(document (TeXmacs")))

(define-format stm
  (:name "TeXmacs Scheme")
  (:suffix "stm")
  (:must-recognize stm-recognizes?))

(converter texmacs-tree stm-document
  (:function texmacs->stm))

(converter stm-document texmacs-tree
  (:function stm->texmacs))

(converter texmacs-tree stm-snippet
  (:function texmacs->stm))

(converter stm-snippet texmacs-tree
  (:function stm-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format code
  (:name "Source Code"))

(tm-define (texmacs->code t . enc)
  (if (null? enc) (set! enc (list (get-locale-charset))))
  (if (tree? t)
      (cpp-texmacs->verbatim t #f (car enc))
      (texmacs->code (tm->tree t) (car enc))))

(tm-define (code->texmacs x . opts)
  (verbatim->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(tm-define (code-snippet->texmacs x . opts)
  (verbatim-snippet->texmacs x (acons "verbatim->texmacs:encoding" "SourceCode" '())))

(converter code-snippet texmacs-tree
  (:function code-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->verbatim x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "texmacs->verbatim:wrap") "on"))
         (enc (or (assoc-ref opts "texmacs->verbatim:encoding") "auto")))
    (cpp-texmacs->verbatim x wrap? enc)))

(tm-define (texmacs->verbatim-snippet x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "texmacs->verbatim:wrap") "on"))
         (enc (or (assoc-ref opts "texmacs->verbatim:encoding") "auto")))
    (if (or (== (get-env "mode") "prog") (== (get-env "font-family") "tt"))
        ;; FIXME: dirty hacks for "copy to verbatim" of code snippets
        (let ((conv (cpp-texmacs->verbatim x #f enc))
              (tick (cpp-texmacs->verbatim (tm->tree "`") #f enc)))
          (string-replace conv tick "`"))
        (cpp-texmacs->verbatim x wrap? enc))))

(tm-define (verbatim->texmacs x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "verbatim->texmacs:wrap") "on"))
         (enc (or (assoc-ref opts "verbatim->texmacs:encoding") "auto")))
    (cpp-verbatim->texmacs x wrap? enc)))

(tm-define (verbatim-snippet->texmacs x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let* ((wrap? (== (assoc-ref opts "verbatim->texmacs:wrap") "on"))
         (enc (or (assoc-ref opts "verbatim->texmacs:encoding") "auto")))
    (cpp-verbatim-snippet->texmacs x wrap? enc)))

(define-format verbatim
  (:name "Verbatim"))
;;(:suffix "txt"))

(converter verbatim-document texmacs-tree
  (:function-with-options verbatim->texmacs)
  (:option "verbatim->texmacs:wrap" "off")
  (:option "verbatim->texmacs:encoding" "auto"))

(converter verbatim-snippet texmacs-tree
  (:function-with-options verbatim-snippet->texmacs)
  (:option "verbatim->texmacs:wrap" "off")
  (:option "verbatim->texmacs:encoding" "auto"))

(converter texmacs-tree verbatim-document
  (:function-with-options texmacs->verbatim)
  (:option "texmacs->verbatim:wrap" "off")
  (:option "texmacs->verbatim:encoding" "auto"))

(converter texmacs-tree verbatim-snippet
  (:function-with-options texmacs->verbatim-snippet)
  (:option "texmacs->verbatim:wrap" "off")
  (:option "texmacs->verbatim:encoding" "auto"))
