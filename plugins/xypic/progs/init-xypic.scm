
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-xypic.scm
;; DESCRIPTION : Initialize XYpic plugin
;; COPYRIGHT   : (C) 2004 Nicolas Ratier.
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xypic-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(define (xypic-launcher)
  (if (url-exists? "$TEXMACS_HOME_PATH/plugins/tmpy")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_HOME_PATH")
                     "/plugins/tmpy/session/tm_xypic.py\"")
      (string-append (python-command) " \""
                     (getenv "TEXMACS_PATH")
                     "/plugins/tmpy/session/tm_xypic.py\"")))

(define (xypic-exists?)
  (and (url-exists-in-path? "latex")
       (cond ((url-exists-in-path? "kpsewhich")
              (!= (eval-system "kpsewhich xy.sty") ""))
             (else #f))))

(plugin-configure xypic
  (:require (python-command))
  (:require (xypic-exists?))
  (:launch ,(xypic-launcher))
  (:serializer ,xypic-serialize)
  (:session "XYpic"))
