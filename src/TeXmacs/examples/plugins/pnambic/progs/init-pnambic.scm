
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pnambic.scm
;; DESCRIPTION : Initialize pnambic plugin
;; COPYRIGHT   : (C) 2005  Andrey Grozin
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure pnambic
  (:require (url-exists-in-path? "pnambic"))
  (:launch "pnambic ~/.TeXmacs/plugins/pnambic/in ~/.TeXmacs/plugins/pnambic/out")
  (:session "Pnambic"))
