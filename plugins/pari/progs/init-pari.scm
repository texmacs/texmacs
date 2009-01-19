
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pari.scm
;; DESCRIPTION : Initialize pari plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-menu (pari-menus) pari-menu)
(define (cas-supports-completions-set! must-be-pari) (noop)) ;; obsolete

(define (pari-initialize)
  (import-from (utils plugins plugin-convert))
  (plugin-input-converters pari)
  (menu-extend texmacs-extra-menu
    (if (or (in-pari?) (and (not-in-session?) (pari-scripts?)))
	(=> "Pari"
	    (link pari-menu)))))

(define (pari-commander s)
  (string-append (char->string #\002)
		 "special:" s
		 (char->string #\005) "\n"))

(plugin-configure pari
  (:require (url-exists-in-path? "gp"))
  (:initialize (pari-initialize))
  (:launch "gp --texmacs")
  (:session "Pari")
  (:scripts "Pari")
  (:tab-completion #t)
  (:commander ,pari-commander))

