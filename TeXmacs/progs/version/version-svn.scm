
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

(define (remove-empty-strings l)
  (cond ((null? l) l)
        ((== (car l) "") (remove-empty-strings (cdr l)))
        (else (cons (car l) (remove-empty-strings (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-status name)
  (:require (== (version-tool name) "svn"))
  (let* ((name-s (url->string name))
         (cmd (string-append "svn status " name-s))
         (ret (eval-system cmd)))
    (cond ((== ret "") "unmodified")
          ((string-starts? ret "I") "unknown")
          ((string-starts? ret "?") "unknown")
          ;; FIXME: support further possibilities
          (else "modified"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (svn-history-item s)
  (let* ((decoded (utf8->cork s))
         (lines (remove-empty-strings (string-decompose decoded "\n")))
         (data (string-decompose (car lines) " | "))
         (date-info (string-decompose (caddr data) " "))
         (msg (string-recompose (cdr lines) " ")))
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
;; File revisions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-revision name rev)
  (:require (== (version-tool name) "svn"))
  ;;(display* "Loading revision " rev " of " name "\n")
  (let* ((name-s (url->string name))
         (cmd (string-append "svn cat -r " rev " " name-s))
         (ret (eval-system cmd)))
    ;;(display* "Got " ret "\n")
    ret))

(tm-define (version-beautify-revision name rev)
  (:require (== (version-tool name) "svn"))
  rev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating, registering committing a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-update name)
  (:require (== (version-tool name) "svn"))
  (let* ((name-s (url->string name))
         (cmd (string-append "svn up --accept theirs-full " name-s))
         (ret (eval-system cmd))
         (l (remove-empty-strings (string-decompose ret "\n"))))
    ;;(display* "ret= " ret "\n")
    (if (null? l) "" (cAr l))))

(tm-define (version-register name)
  (:require (== (version-tool name) "svn"))
  (let* ((name-s (url->string name))
         (cmd (string-append "svn add " name-s))
         (ret (eval-system cmd))
         (l (remove-empty-strings (string-decompose ret "\n"))))
    (if (null? l) "" (cAr l))))

(tm-define (version-unregister name)
  (:require (== (version-tool name) "svn"))
  (let* ((name-s (url->string name))
         (cmd (string-append "svn remove --force " name-s))
         (ret (eval-system cmd))
         (l (remove-empty-strings (string-decompose ret "\n"))))
    (if (null? l) "" (cAr l))))

(tm-define (version-commit name msg)
  (:require (== (version-tool name) "svn"))
  (let* ((name-s (url->string name))
         (msg-s (string-replace msg "\"" "\\\""))
         (encoded (cork->utf8 msg-s))
         (cmd (string-append "svn commit -m \"" encoded "\" " name-s))
         (ret (eval-system cmd))
         (l (remove-empty-strings (string-decompose ret "\n"))))
    (if (null? l) "" (cAr l))))
