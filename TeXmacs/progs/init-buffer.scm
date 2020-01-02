
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-buffer.scm
;; DESCRIPTION : This file is executed when creating a new buffer
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (buffer-newly-created? (current-buffer))
  (use-modules (generic document-edit) (texmacs texmacs tm-print))
  (init-style "generic")
  (with lan (get-preference "language")
    (if (!= lan "english") (set-document-language lan)))
  (with psz (get-printer-paper-type)
    (if (!= psz "a4") (init-page-type psz)))
  (with type (get-preference "page medium")
    (if (!= type "papyrus") (init-env "page-medium" type)))
  (when (!= (get-preference "scripting language") "none")
    (lazy-plugin-force)
    (init-env "prog-scripts" (get-preference "scripting language")))
  (buffer-pretend-saved (current-buffer))
  (buffer-initialized (current-buffer)))
