
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-tmml.scm
;; DESCRIPTION : setup tmml converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tmml init-tmml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML format for TeXmacs (no information loss)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmml-recognizes-at? s pos xml?)
  (set! pos (format-skip-spaces s pos))
  (cond ((format-test? s pos "<texmacs") xml?)
	((format-test? s pos "<?xml ")
	 (tmml-recognizes-at? s (format-skip-line s pos) #t))
	(else #f)))

(define (tmml-recognizes? s)
  (and (string? s) (tmml-recognizes-at? s 0 #f)))

(define-format tmml
  (:name "Xml")
  (:suffix "tmml")
  (:must-recognize tmml-recognizes?))

(lazy-define (convert tmml tmmltm) parse-tmml)
(lazy-define (convert tmml tmmlout) serialize-tmml)
(lazy-define (convert tmml tmmltm) tmml->texmacs)
(lazy-define (convert tmml tmtmml) texmacs->tmml)

(converter tmml-document tmml-stree
  (:function parse-tmml))

(converter tmml-stree tmml-document
  (:function serialize-tmml))

(converter tmml-snippet tmml-stree
  (:function parse-tmml))

(converter tmml-stree tmml-snippet
  (:function serialize-tmml))

(converter tmml-stree texmacs-stree
  (:function tmml->texmacs))

(converter texmacs-stree tmml-stree
  (:function texmacs->tmml))
