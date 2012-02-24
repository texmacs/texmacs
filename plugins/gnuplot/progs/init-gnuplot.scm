
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-gnuplot.scm
;; DESCRIPTION : Initialize GNUplot plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnuplot-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (gnuplot-input) gnuplot))

(define (gnuplot-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(plugin-configure gnuplot
  (:require (url-exists-in-path? "gnuplot"))
  (:initialize (gnuplot-initialize))
  (:launch "tm_gnuplot --texmacs")
  (:serializer ,gnuplot-serialize)
  (:session "Gnuplot")
  (:scripts "Gnuplot"))
