
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-sage.scm
;; DESCRIPTION : Initialize SAGE plugin
;; COPYRIGHT   : (C) 2004  Ero Carrera
;; COPYRIGHT   : (C) 2007  Mike Carrera
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure sage
  (:require (url-exists-in-path? "sage"))
  (:require (url-exists-in-path? "tm_sage"))
  (:launch "sage -python `which tm_sage`")
  (:tab-completion #t)
  (:session "Sage"))
