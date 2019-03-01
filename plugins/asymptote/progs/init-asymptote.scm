
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
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(plugin-configure asymptote
  (:require (and (not (os-mingw?)) (url-exists-in-path? "asy")))
  (:launch "tm_asy")
  (:serializer ,asy-serialize)
  (:session "Asymptote")
  (:scripts "Asymptote"))

(when (supports-asymptote?)
  (import-from (asymptote-menus))
  (import-from (utils plugins plugin-convert)))
