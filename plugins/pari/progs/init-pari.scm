
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pari.scm
;; DESCRIPTION : Initialize pari plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-menu (pari-menus) pari-menu)

(define (pari-initialize)
  (import-from (utils plugins plugin-convert))
  (plugin-input-converters pari)
  (menu-extend texmacs-extra-menu
    (if (or (in-pari?) (test-env? "prog-scripts" "pari"))
	(=> "Pari"
	    (link pari-menu)))))

(define (cas-supports-completions-set! must-be-pari)
  ;; Hack for old versions of Pari
  (define (pari-commander s)
    (string-append (char->string #\002)
		   "special:" s
		   (char->string #\005) "\n"))
  (plugin-configure pari
    (:tab-completion #t)
    (:commander ,pari-commander)))

(plugin-configure pari
  (:require (url-exists-in-path? "gp"))
  (:initialize (pari-initialize))
  (:launch "gp --texmacs")
  (:session "Pari")
  (:scripts "Pari"))
