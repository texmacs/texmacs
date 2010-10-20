
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : selections.scm
;; DESCRIPTION : selection routines
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit selections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of built-in routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (clipboard-copy to)
  (:argument to "Copy to")
  (:default  to "primary"))

(tm-property (clipboard-cut to)
  (:argument to "Cut to")
  (:default  to "primary"))

(tm-property (clipboard-paste from)
  (:argument from "Paste from")
  (:default  to "primary"))

(define (clipboard-test-import? s)
  (string=? s (clipboard-get-import)))

(tm-property (clipboard-set-import s)
  (:check-mark "*" clipboard-test-import?))

(define (clipboard-test-export? s)
  (string=? s (clipboard-get-export)))

(tm-property (clipboard-set-export s)
  (:check-mark "*" clipboard-test-export?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exporting and importing selections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (clipboard-copy-export format which)
  (let ((temp (clipboard-get-export)))
    (clipboard-set-export format)
    (clipboard-copy which)
    (clipboard-set-export temp)))

(tm-define (clipboard-cut-export format which)
  (let ((temp (clipboard-get-export)))
    (clipboard-set-export format)
    (clipboard-cut which)
    (clipboard-set-export temp)))

(tm-define (clipboard-paste-import format which)
  (let ((temp (clipboard-get-import)))
    (clipboard-set-import format)
    (clipboard-paste which)
    (clipboard-set-import temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured selections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-select-enlarge)
  (if (selection-active-enlarging?)
      (select-enlarge)
      (begin
	(selection-cancel)
	(selection-set-start)
	(select-from-keyboard #t))))

(tm-define (kbd-select-environment)
  (if (selection-active-enlarging?)
      (select-enlarge-environmental)
      (begin
	(selection-cancel)
	(selection-set-start)
	(select-from-keyboard #t)
	(select-enlarge-environmental))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful macros for operating on selections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (wrap-selection-any ,@actions)
  `(if (selection-active-any)
       (begin
	 (clipboard-cut "wrapbuf")
	 ,@actions
	 (clipboard-paste "wrapbuf"))
       (begin
	 ,@actions)))

(tm-define-macro (wrap-selection-small ,@actions)
  `(if (selection-active-small)
       (begin
	 (clipboard-cut "wrapbuf")
	 ,@actions
	 (clipboard-paste "wrapbuf"))
       (begin
	 ,@actions)))
