
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-svn.scm
;; DESCRIPTION : support for external versioning tools
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-svn)
  (:use (version version-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define svn-sep "------------------------------------------------------------------------\n")

(define (dos->unix s)
  (string-replace s "\r\n" "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File history using SVN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (svn-history-item s)
  (let* ((lines (string-decompose s "\n"))
         (data (string-decompose (car lines) " | "))
         (date-info (string-decompose (caddr data) " "))
         (msg-lines (cDDr (cddr lines)))
         (msg (string-recompose msg-lines " ")))
    (list (car data) (cadr data) (car date-info) msg)))

(tm-define (version-history name)
  (:require (== (version-tool name) "svn"))
  (let* ((name-s (url->string name))
         (cmd (string-append "svn log " name-s))
         (s (dos->unix (eval-system cmd)))
         (l (string-decompose s svn-sep)))
    (and (> (length l) 1)
         (== (car l) "")
         (== (cAr l) "")
         (map svn-history-item (cdr (cDr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File releases using SVN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-release name rel)
  (:require (== (version-tool name) "svn"))
  ;;(display* "Loading release " rel " of " name "\n")
  (let* ((name-s (url->string name))
         (nr (substring rel 1 (string-length rel)))
         (cmd (string-append "svn cat -r " nr " " name-s))
         (ret (eval-system cmd)))
    ;;(display* "Got " ret "\n")
    ret))
