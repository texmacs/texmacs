;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-haskell.scm
;; DESCRIPTION : Initialize hp
;; COPYRIGHT   : (C) 2019  B.Bratschi
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define (haskell-serialize lan t)
;;  (import-from (utils plugins plugin-cmd))
;;  (with u (pre-serialize lan t)
;;    (with s (texmacs->verbatim (stree->tree u))
;;      (string-append (escape-verbatim (string-replace s "\n" "~")) "\n"))))


(define (haskell-serialize lan t)
  (with u (pre-serialize lan t)
    (with s (texmacs->code (stree->tree u))
      (string-append  (string-replace (string-replace (string-replace s "\\" "\\\\") "\"" "''")  "\n" "~") "\n")
      )))
      
(define (haskell-initialize)
  (import-from (haskell-menu))
  (import-from (utils plugins plugin-convert))
  (menu-extend texmacs-extra-menu
	(if (or (in-haskell?) (and (not-in-session?) (haskell-scripts?)))
		(=> "haskell" (link haskell-menu)))))

(plugin-configure haskell
(:require (url-exists-in-path? "TeXmacsGhci"))
(:require (url-exists-in-path? "HaskellPlugin"))
(:initialize (haskell-initialize))
(:launch "HaskellPlugin")
(:serializer ,haskell-serialize)
(:session "Haskell"))
