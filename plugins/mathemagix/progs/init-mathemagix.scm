
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mathemagix.scm
;; DESCRIPTION : Initialize mathemagix plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, 2012  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format mathemagix
  (:name "Mathemagix Source Code")
  (:suffix "mmx" "mmh"))

(define (texmacs->mathemagix x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (mathemagix->texmacs x . opts)
  (code->texmacs x))

(define (mathemagix-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree mathemagix-document
  (:function texmacs->mathemagix))

(converter mathemagix-document texmacs-tree
  (:function mathemagix->texmacs))
  
(converter texmacs-tree mathemagix-snippet
  (:function texmacs->mathemagix))

(converter mathemagix-snippet texmacs-tree
  (:function mathemagix-snippet->texmacs))


(define (mathemagix-serialize lan t)
  (with u (pre-serialize lan t)
    (with v (texmacs->code u)
      (with w (string-replace v "\n" "/{CR}/")
        (string-append (escape-verbatim w) "\n")))))

(plugin-configure mathemagix
  (:winpath "mathemagix*" "bin")
  (:winpath "Mathemagix*" "bin")
  (:require (url-exists-in-path? "mmx-light"))
  (:serializer ,mathemagix-serialize)
  (:launch "mmx-light --texmacs")
  (:session "Mathemagix")
  (:scripts "Mathemagix"))

(texmacs-modes
  (in-mathemagix% (== (get-env "prog-language") "mathemagix"))
  (in-prog-mathemagix% #t in-prog% in-mathemagix%)
  (in-mathemagix-math% #t in-mathemagix% in-math%)
  (mathemagix-scripts-math% #t mathemagix-scripts% in-math%))

(lazy-keyboard (mathemagix-edit) in-prog-mathemagix?)

(when (supports-mathemagix?)
  (import-from (mathemagix-menus))
  (lazy-input-converter (mathemagix-input) mathemagix)
  (lazy-keyboard (mathemagix-kbd) in-mathemagix?)
  (plugin-approx-command-set! "mathemagix" ""))
