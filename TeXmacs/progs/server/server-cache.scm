
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-cache
;; DESCRIPTION : TeXmacs server tree cache
;; COPYRIGHT   : (C) 2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-cache))

(tm-define (server-handle-cache sname uid t)
  (if (client-version>=? uid 1) (tree-cache-update sname t) t))

(tm-service (remote-get-cache-ref host ref)
  ;;(display* "remote-file-load " rname "\n")
  (with uid (server-get-user envelope)
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          (else
            (server-return envelope (tree->stree (tree-cache-get host ref)))))))

