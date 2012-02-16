
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-eukleides.scm
;; DESCRIPTION : Initialize Eukleides plugin
;; COPYRIGHT   : (C) 2003 Joris van der Hoeven.
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eukleides-initialize)
  (import-from (eukleides-menus))
  (import-from (utils plugins plugin-convert)))

(define (eukleides-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->verbatim (stree->tree u))
      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))

(plugin-configure eukleides
  (:require (url-exists-in-path? "eukleides"))
  (:initialize (eukleides-initialize))
  (:launch "tm_eukleides --texmacs")
  (:serializer ,eukleides-serialize)
  (:session "Eukleides")
  (:scripts "Eukleides"))

