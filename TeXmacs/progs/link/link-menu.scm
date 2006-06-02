
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-menu.scm
;; DESCRIPTION : linking portions of text
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-menu)
  (:use (link link-edit)))

(menu-bind link-menu
  ("New locus" (make-locus))
  (when (inside? 'locus)
    ("Set source" (link-insert-locus 0))
    ("Set destination" (link-insert-locus 1)))
  (when (link-under-construction?)
    ("Create link" (interactive make-link)))
  (when (link-may-follow?)
    ("Follow link" (link-follow))))
