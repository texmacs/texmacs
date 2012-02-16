
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-r.scm
;; DESCRIPTION : Initialize GNU R plugin
;; COPYRIGHT   : (C) 1999  Michael Lachmann and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (r-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (r-input) r))

(define (r-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append (escape-verbatim 
		      (string-replace s "\n" ";;")) "\n"))))

(plugin-configure r
  (:require (url-exists-in-path? "R"))
  (:initialize (r-initialize))
  (:serializer ,r-serialize)
  (:launch "exec tm_r")
  (:tab-completion #t)
  (:session "R")
  (:scripts "R"))

(menu-bind r-menu
  ("update menu" (insert "t.update.menus(max.len=30)"))
  ("R help in TeXmacs" (insert "t.start.help()")))

(menu-bind plugin-menu
  (:require (in-r?))
  (=> "R" (link r-menu)))
