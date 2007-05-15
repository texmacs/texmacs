
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chat-menu.scm
;; DESCRIPTION : menus for chatting
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote chat-menu)
  (:use (remote chat-edit)))

(define (chat-administrated-menu-entry name)
  (list name (lambda () (chat-connect name))))

(tm-define (chat-administrated-menu)
  (with l (chat-list-administrated-rooms)
    (if (not l) (menu-dynamic)
	(with sorted-l (list-sort l string<=?)
	  (menu-dynamic
	    ---
	    ,@(map chat-administrated-menu-entry sorted-l))))))

(define (chat-connect-menu-entry name)
  (list name (lambda () (chat-connect name))))

(tm-define (chat-connect-menu)
  (with l (chat-list-rooms)
    (if (not l) (menu-dynamic)
	(with sorted-l (list-sort l string<=?)
	  (menu-dynamic
	    ,@(map chat-connect-menu-entry sorted-l)
	    ---)))))

(menu-bind chat-menu
  (if (not (chat-session))
      (-> "Connect"
	  ("Create chatroom" (interactive chatroom-create))
	  (link chat-administrated-menu)
	  ---
	  (link chat-connect-menu)
	  ("Other" (interactive chat-connect))))
  (if (chat-session)
      (when (not (chat-connected?))
	("Catch up" (chat-catch-up))))
  (when (chat-connected?)
    ("Hang up" (interactive chat-hang-up))))
