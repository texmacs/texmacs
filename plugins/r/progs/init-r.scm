
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

(define (r-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim 
		      (string-replace s "\n" ";;")) "\n"))))

(define (r-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/r")
      (setenv "TEXMACS_SEND"
              "source(paste(Sys.getenv(\"TEXMACS_HOME_PATH\"),\"/plugins/r/texmacs.r\",sep=\"\"))\n"))
  "tm_r")

(plugin-configure r
  (:winpath "R-*" "bin")
  (:winpath "R/R*" "bin")
  (:require (url-exists-in-path? "R"))
  (:serializer ,r-serialize)
  (:launch ,(r-launcher))
  (:tab-completion #t)
  (:session "R")
  (:scripts "R"))

(texmacs-modes
  (in-r% (== (get-env "prog-language") "r"))
  (in-prog-r% #t in-prog% in-r%))

(lazy-keyboard (r-edit) in-prog-r?)

(when (supports-r?)
  (lazy-input-converter (r-input) r)

  (menu-bind r-menu
    ("update menu" (insert "t.update.menus(max.len=30)"))
    ("R help in TeXmacs" (insert "t.start.help()")))

  (menu-bind plugin-menu
    (:require (in-r?))
    (=> "R" (link r-menu))))
