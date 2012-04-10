
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

(define reduce-help #f)

(define (reduce-initialize)
  (import-from (utils plugins plugin-convert))
  (import-from (utils plugins plugin-cmd))
  (import-from (dynamic session-menu))
  (import-from (reduce-menus))
  (lazy-input-converter (reduce-input) reduce)
  (let ((help (getenv "REDUCE_HELP")))
    (if (and help (url-exists? help)) (set! reduce-help help))))

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
