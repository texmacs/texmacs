
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-reduce.scm
;; DESCRIPTION : Initialize reduce plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reduce-initialize)
  (import-from (utils plugins plugin-convert))
  (import-from (reduce-menus))
  (lazy-input-converter (reduce-input) reduce)
  (menu-extend session-help-icons
    (if (and (in-reduce?) (url-exists? "/home/grozin/reduce-1556/cslbuild/i686-pc-linux-gnu/csl/reduce.doc/index.html"))
	|
	(=> (balloon (icon "tm_help.xpm") "Reduce documentation")
	    (link reduce-help-menu))))
  (menu-extend texmacs-extra-menu
    (if (or (in-reduce?) (and (not-in-session?) (reduce-scripts?)))
	(=> "Reduce"
	    (link reduce-menu)))))

(define (reduce-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (string-drop-right (verbatim-serialize lan t) 1)
    (cond ((== s "") "0;\n")
          ((in? (string-ref s (- (string-length s) 1)) '(#\; #\$))
           (string-append s "\n"))
          (else (string-append s ";\n")))))

(plugin-configure reduce
  (:require (url-exists-in-path? "redpsl"))
  (:initialize (reduce-initialize))
  (:launch "tm_reduce")
  (:serializer ,reduce-serialize)
  (:session "Reduce")
  (:scripts "Reduce"))
