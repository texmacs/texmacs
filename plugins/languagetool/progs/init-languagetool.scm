
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-languagetool.scm
;; DESCRIPTION : Initialize languagetool plugin
;; COPYRIGHT   : (C) 2026  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("languagetool server" "http://localhost:8081"
   (lambda (var val) (noop)))
  ("languagetool use widgets" "on"
   (lambda (var val) (noop)))
  ("languagetool premium" "off"
   (lambda (var val) (noop)))
  ("languagetool username" ""
   (lambda (var val) (noop)))
  ("languagetool API key" ""
   (lambda (var val) (noop))))

(tm-widget (plugin-preferences-widget name)
  (:require (and (== name "languagetool")
		 (get-boolean-preference "grammar checking")))
  (aligned
    (item (hlist // (text "Server URL:"))
      (enum (set-preference "languagetool server" answer)
	    '("http://localhost:8081" "https://api.languagetool.org" "")
	    (get-preference "languagetool server") "14em"))
    (meti (hlist // (text "Use widgets"))
      (toggle (begin (set-boolean-preference
		      "languagetool use widgets" answer)
		     (refresh-now "languagetool use widgets"))
	      (get-boolean-preference "languagetool use widgets")))
    (meti (hlist // (text "Use Premium access"))
      (toggle (begin (set-boolean-preference
		      "languagetool premium" answer)
		     (refresh-now "languagetool premium"))
	      (get-boolean-preference "languagetool premium"))))
  (refreshable "languagetool premium"
    (when (get-boolean-preference "languagetool premium")
      (aligned
	(item (hlist // (text "Username:"))
	  (enum (set-preference "languagetool username" answer)
		'((get-preference "languagetool username") "")
		(get-preference "languagetool username") "14em"))
	(item (hlist // (text "API key:"))
	  (enum (set-preference "languagetool API key" answer)
		'((get-preference "languagetool API key") "")
		(get-preference "languagetool API key") "14em"))))))
      
(plugin-configure languagetool
  (:require (get-boolean-preference "grammar checking"))
  (:preferences #t))
