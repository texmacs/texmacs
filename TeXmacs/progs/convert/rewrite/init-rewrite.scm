
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-rewrite.scm
;; DESCRIPTION : setup texmacs converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

(define (scheme-recognizes? s)
  (and (string? s) (string-starts? s "(document (TeXmacs")))

(define-format scheme
  (:name "Scheme")
  (:suffix "scm")
  (:must-recognize scheme-recognizes?))

(converter texmacs-tree scheme-document
  (:function texmacs->scheme))

(converter scheme-document texmacs-tree
  (:function scheme->texmacs))

(converter texmacs-tree scheme-snippet
  (:function texmacs->scheme))

(converter scheme-snippet texmacs-tree
  (:function scheme-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format verbatim
  (:name "Verbatim")
  (:suffix "txt"))

(converter verbatim-document texmacs-tree
  (:function verbatim->texmacs))

(converter texmacs-tree verbatim-document
  (:function texmacs->verbatim))

(converter verbatim-snippet texmacs-tree
  (:function verbatim-snippet->texmacs))

(converter texmacs-tree verbatim-snippet
  (:function texmacs->verbatim))
