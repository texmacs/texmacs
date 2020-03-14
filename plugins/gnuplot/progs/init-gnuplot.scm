
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-gnuplot.scm
;; DESCRIPTION : Initialize GNUplot plugin
;; COPYRIGHT   : (C) 1999       Joris van der Hoeven
;;                   2019-2020  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (python-command)
  (if (url-exists-in-path? "python3") "python3" "python2"))

(define (python-exists?)
  (or (url-exists-in-path? "python3")
      (url-exists-in-path? "python2")))

(define (gnuplot-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (gnuplot-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      `((:launch ,(string-append (python-command) " \""
                                 (getenv "TEXMACS_HOME_PATH")
                                 "/plugins/tmpy/session/tm_gnuplot.py\"")))
      `((:launch ,(string-append (python-command) " \""
                                 (getenv "TEXMACS_PATH")
                                 "/plugins/tmpy/session/tm_gnuplot.py\"")))))


(plugin-configure gnuplot
  (:require (url-exists-in-path? "gnuplot"))
  (:require (python-exists))
  ,@(gnuplot-launcher)
  (:serializer ,gnuplot-serialize)
  (:session "Gnuplot")
  (:scripts "Gnuplot"))

(when (supports-gnuplot?)
  (lazy-input-converter (gnuplot-input) gnuplot))
