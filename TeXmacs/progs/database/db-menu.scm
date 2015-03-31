
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-menu.scm
;; DESCRIPTION : menus for searching and managing databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-menu)
  (:use (database db-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The database toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-toolbar-search search)
  (display* "Searching " search "\n"))

(define (db-toolbar-limit limit)
  (display* "Set limit to " limit "\n"))

(tm-widget (db-toolbar)
  (hlist
    (text "Search: ")
    (input (db-toolbar-search answer) "db-search"
           (list "") "25em")
    // // //
    (text "Limit: ")
    (input (db-toolbar-limit answer) "db-limit"
           (list "10" "25" "100" "250" "") "3em")
    >> >> >> >> >> >> >> >>
    (glue #t #f 0 24)))
