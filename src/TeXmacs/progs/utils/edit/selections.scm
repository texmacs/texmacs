
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : selections.scm
;; DESCRIPTION : selection routines
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit selections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checkmarks for the current import/export formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
