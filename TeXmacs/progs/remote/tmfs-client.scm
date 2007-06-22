
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs-client.scm
;; DESCRIPTION : clients of remote TeXmacs file systems
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs-client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit and checkout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmfs-checkout l)
  (dialogue
    (let* ((new-closure (tmfs-client-remote `(tmfs-closure ,l)))
	   (old-closure (tmfs-closure l))
	   (diff-closure (list-difference new-closure old-closure))
	   (news (tmfs-client-remote `(tmfs-get-ressources ,diff-closure))))
      (tmfs-set-ressources news))))

(tm-define (tmfs-commit l)
  (dialogue
    (let* ((old-closure (tmfs-client-remote `(tmfs-closure ,l)))
	   (new-closure (tmfs-closure l))
	   (diff-closure (list-difference new-closure old-closure))
	   (news (tmfs-get-ressources diff-closure)))
      (tmfs-client-remote `(tmfs-set-ressources ,news)))))
