
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-asymptote.scm
;; DESCRIPTION : Initialize Asymptote plugin
;; COPYRIGHT   : (C) Yann Dirson <ydirson at altern dot org>.
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (asy-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(define (asymptote-initialize)
  (import-from (asymptote-menus))
  (import-from (utils plugins plugin-convert))
  (menu-extend texmacs-extra-menu
	(if (or (in-asymptote?) (and (not-in-session?) (asymptote-scripts?)))
		(=> "Asymptote" (link asymptote-functions-menu)))))

(plugin-configure asymptote
                  (:require (url-exists-in-path? "asy"))
                  (:launch "tm_asy2")
		  (:serializer ,asy-serialize)
		  (:initialize (asymptote-initialize))
                  (:session "Asymptote")
                  (:scripts "Asymptote")
                  (:version "0.3")
                  )
