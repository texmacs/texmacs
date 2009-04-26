
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-eukleides.scm
;; DESCRIPTION : Initialize Eukleides plugin
;; COPYRIGHT   : (C) 2003 Joris van der Hoeven.
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eukleides-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(define (eukleides-initialize)
  (import-from (eukleides-menus))
  (import-from (utils plugins plugin-convert))
  (menu-extend texmacs-extra-menu
	(if (or (in-eukleides?) (and (not-in-session?) (eukleides-scripts?)))
		(=> "Eukleides" (link eukleides-functions-menu)))))

(plugin-configure eukleides
  (:require (url-exists-in-path? "eukleides"))
  (:initialize (eukleides-initialize))
  (:launch "tm_eukleides --texmacs")
  (:serializer ,eukleides-serialize)
  (:session "Eukleides")
  (:scripts "Eukleides"))

