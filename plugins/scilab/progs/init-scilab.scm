
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-scilab.scm
;; DESCRIPTION : Initialize scilab plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scilab-launcher (if(os-mingw?) "Scilex -texmacs" "scilab --texmacs"))

(plugin-configure scilab
  (:winpath ,(url-append (url-wildcard "scilab*") "bin"))
  (:require (url-exists-in-path? (if(os-mingw?) "Scilex" "scilab")))
  (:launch ,scilab-launcher)
  (:session "Scilab"))
