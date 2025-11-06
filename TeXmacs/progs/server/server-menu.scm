
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-menu.scm
;; DESCRIPTION : menus for remote TeXmacs services
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;                   2022  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-menu)
  (:use (server server-widgets)
        (server server-db)
        (server server-tmfs)
        (server server-sync)
        (server server-chat)))

(tm-define (server-safe-start)
  (if (supports-gnutls?)
    (if (server-certificate-exists?)
      (server-start)
      (open-certificate-warning "No certificate detected." #f))
    (begin
      (server-start)
      (server-log-write `log-warning "started server without TLS support"))))

(menu-bind server-start-menu
  ("Start server" (server-safe-start)))

(menu-bind server-menu
  (group (eval (string-append "Server running on port "
			      (number->string (server-port-in-use)))))
  ("Stop server" (server-stop))
  ("Restart server" (server-stop) (sleep 1) (server-start))
  ("Reset admin password" (server-reset-admin-password))
  ("Reset preferences"
   (server-reset-preferences)
   (server-open-success "Preferences have been reset.")))
