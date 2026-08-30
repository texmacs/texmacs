
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-agents-menu.scm
;; DESCRIPTION : extra menus for editing AI agents
;; COPYRIGHT   : (C) 2026  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database ai-agents-menu)
  (:use (database db-menu)
        (database ai-agents-db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve kind of AI agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-ai-agents% (style-has? "database-ai-agents-style")))

(tm-define (db-get-kind)
  (:mode in-ai-agents?)
  "ai-agents")

(tm-define (open-ai-agents)
  (load-db-buffer "tmfs://db/ai-agents/global"))

