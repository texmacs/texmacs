
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
  (let* ((old (url->unix buf))
	 (new (string-append "tmfs://part" old "/" incl)))
    (load-buffer new)))

(define (drop-tmfs-data u)
  (with s (url->string u)
    (if (string-starts? s "tmfs://part/")
	(unix->url (string-drop s (string-length "tmfs://part")))
	u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dynamic document part menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-master-menu
  (if (buffer-has-preamble?)
      ("Show preamble" (buffer-set-part-mode :preamble)))
  (if (not (buffer-has-preamble?))
      ("Create preamble" (buffer-make-preamble)))
  ("Show main document" (buffer-set-part-mode :all))
  ---
  (for (incl (buffer-get-includes))
    ((eval incl) (goto-include (current-buffer) incl))))

(menu-bind document-part-menu
  (:require (url-rooted-tmfs-protocol? (current-buffer) "part"))
  (let* ((u (drop-tmfs-data (current-buffer)))
         (m (part-master u))
         (f (part-file u))
         (t (tree-import m "texmacs"))
	 (b (tmfile-get t 'body))
	 (l (tm-get-includes b)))
    ((eval (url->string (url-tail m))) (load-buffer m))
    ---
    (for (incl l)
      ((eval incl) (goto-include m incl)))))
