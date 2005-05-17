
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-buffer.scm
;; DESCRIPTION : This file is executed when creating a new buffer
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (no-name?)
    (begin
      (use-modules (generic document-edit) (texmacs texmacs tm-print))
      (init-style "generic")
      (with lan (get-preference "language")
	(if (!= lan "english") (init-language lan)))
      (with psz (get-printer-paper-type)
	(if (!= psz "a4") (init-page-type psz)))
      (pretend-save-buffer)))
