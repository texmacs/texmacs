
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs-client.scm
;; DESCRIPTION : clients of remote TeXmacs file systems
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs-client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit and checkout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmfs-checkout l)
  (let* ((new-closure (tmfs-client-remote `(tmfs-closure ,l)))
	 (old-closure (tmfs-closure l))
	 (diff-closure (list-difference new-closure old-closure))
	 (news (tmfs-client-remote `(tmfs-get-ressources ,diff-closure))))
    (tmfs-set-ressources news)))

(tm-define (tmfs-commit l)
  (let* ((old-closure (tmfs-client-remote `(tmfs-closure ,l)))
	 (new-closure (tmfs-closure l))
	 (diff-closure (list-difference new-closure old-closure))
	 (news (tmfs-get-ressources diff-closure)))
    (tmfs-client-remote `(tmfs-set-ressources ,news))))
