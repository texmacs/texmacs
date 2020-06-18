;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmarkdown.scm
;; DESCRIPTION : Markdown format for TeXmacs
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown init-markdown))

(define-preferences
  ("texmacs->markdown:hugo-extensions" "off" (lambda (var val) (noop)))
  ("texmacs->markdown:paragraph-width" "79" (lambda (var val) (noop))))

(define-format markdown
  (:name "Markdown")
  (:suffix "md")
  ;(:recognize/:must-recognize markdown-recognizes?))
  )

;(lazy-define (convert markdown markdowntm) parse-markdown)
;(lazy-define (convert markdown markdowntm) markdown->texmacs)

(lazy-define (convert markdown markdownout) serialize-markdown)
(lazy-define (convert markdown markdownout) serialize-markdown-document)
(lazy-define (convert markdown tmmarkdown) texmacs->markdown)

;(converter markdown-document markdown-stree
;  (:function parse-markdown))

(converter markdown-stree markdown-document
  (:function serialize-markdown-document))

;(converter markdown-snippet markdown-stree
;  (:function parse-markdown))

(converter markdown-stree markdown-snippet
  (:function serialize-markdown))

;(converter markdown-stree texmacs-stree
;  (:function markdown->texmacs))

(converter texmacs-stree markdown-stree
  (:function texmacs->markdown))
