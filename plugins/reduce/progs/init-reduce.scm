
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
  (lazy-input-converter (reduce-input) reduce))

(plugin-configure reduce
  (:require (url-exists-in-path? "reduce"))
  (:initialize (reduce-initialize))
  (:launch "tm_reduce")
  (:session "Reduce"))
