
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-cursor.scm
;; DESCRIPTION : routines for cursor movement
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs tools tm-cursor)
  (:export
    cursor-left cursor-right cursor-up cursor-down
    cursor-page-up cursor-page-down cursor-start-line cursor-end-line
    cursor-select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (cursor-left)
  (if (inside? "input") (session-go-left) (go-left)))

(tm-define (cursor-right)
  (if (inside? "input") (session-go-right) (go-right)))

(tm-define (cursor-up)
  (if (inside? "input") (session-go-up) (go-up)))

(tm-define (cursor-down)
  (if (inside? "input") (session-go-down) (go-down)))

(tm-define (cursor-page-up)
  (if (inside? "input") (session-go-page-up) (go-page-up)))

(tm-define (cursor-page-down)
  (if (inside? "input") (session-go-page-down) (go-page-down)))

(tm-define (cursor-start-line) (go-start-line))
(tm-define (cursor-end-line) (go-end-line))

(tm-define (cursor-select r)
  (select-from-shift-keyboard)
  (r)
  (select-from-cursor))
