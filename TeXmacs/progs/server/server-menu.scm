
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-menu.scm
;; DESCRIPTION : menus for remote TeXmacs services
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-menu)
  (:use (server server-base)
        (server server-db)
        (server server-tmfs)
        (server server-sync)
        (server server-chat)))

(menu-bind start-server-menu
  ("Start server" (server-start)))

(menu-bind server-menu
  ("Stop server" (server-stop))
  ("Open licence agreement"
   (load-document "$TEXMACS_HOME_PATH/server/licence.tm"))
  ("Set user information" (interactive server-set-user-information)))
