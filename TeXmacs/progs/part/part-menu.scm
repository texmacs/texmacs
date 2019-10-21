
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : part-menu.scm
;; DESCRIPTION : menus for multi-file documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (part part-menu)
  (:use (generic document-part)
	(part part-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (goto-include buf incl)
  (load-document (part-url buf (url-relative buf incl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dynamic document part menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-master-menu
  (link preamble-menu)
  ---
  (for (incl (buffer-get-includes))
    ((eval incl) (goto-include (current-buffer) incl))))

(menu-bind document-part-menu
  (:require (url-rooted-tmfs-protocol? (current-buffer) "part"))
  (let* ((name (part-open-name (current-buffer)))
         (m (part-master name))
         (f (part-file name))
         (t (tree-import m "texmacs"))
	 (b (tmfile-get t 'body))
	 (l (tm-get-includes b)))
    ((eval (url->string (url-tail m))) (load-document m))
    ---
    (for (incl l)
      ((eval incl) (goto-include m incl)))))
