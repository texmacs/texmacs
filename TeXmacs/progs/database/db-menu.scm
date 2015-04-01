
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
;; Getting and modifying the current database query
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-toolbar-current-search)
  (with a (db-get-current-query (current-buffer))
    (or (assoc-ref a "exact-search") "")))

(define (db-toolbar-search search)
  (let* ((a (db-get-current-query (current-buffer)))
         (keys (compute-keys-string search "verbatim"))
         (s (string-recompose keys ",")))
    (set! a (assoc-set! a "exact-search" search))
    (set! a (assoc-set! a "search" s))
    (db-set-current-query (current-buffer) a)
    (revert-buffer)
    (keyboard-focus-on "db-search")))

(define (db-toolbar-current-order)
  (with a (db-get-current-query (current-buffer))
    (or (assoc-ref a "exact-order") "Name")))

(define (db-toolbar-order order)
  (with a (db-get-current-query (current-buffer))
    (set! a (assoc-set! a "exact-order" order))
    (set! a (assoc-set! a "order" (string-replace (locase-all order) " " "")))
    (db-set-current-query (current-buffer) a)
    (revert-buffer)))

(define (db-toolbar-current-direction)
  (with a (db-get-current-query (current-buffer))
    (or (assoc-ref a "direction") "ascend")))

(define (db-toolbar-direction dir)
  (with a (db-get-current-query (current-buffer))
    (set! a (assoc-set! a "direction" dir))
    (db-set-current-query (current-buffer) a)
    (revert-buffer)))

(define (db-toolbar-current-limit)
  (with a (db-get-current-query (current-buffer))
    (or (assoc-ref a "limit") "10")))

(define (db-toolbar-limit limit)
  (with a (db-get-current-query (current-buffer))
    (set! a (assoc-set! a "limit" limit))
    (db-set-current-query (current-buffer) a)
    (revert-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visibility of the database toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-toolbar-on?)
  (with r (in-database?)
    (when (not r)
      (set! toolbar-db-active? #f)
      (show-bottom-tools 0 #f)
      r)))

(tm-define (db-show-toolbar)
  (delayed
    (:idle 100)
    (set! toolbar-db-active? #t)
    (show-bottom-tools 0 #t)
    (delayed
      (:idle 250)
      (keyboard-focus-on "db-search"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The database toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (db-toolbar)
  (if (db-toolbar-on?)
      (hlist
        (text "Search: ")
        (input (when answer (db-toolbar-search answer))
               "db-search" (list (db-toolbar-current-search)) "25em")
        // // //
        (text "Order: ")
        (with order (db-toolbar-current-order)
          (enum (when answer (db-toolbar-order answer))
                (list order "Name" "Author" "Title" "Year" "Year, Name" "")
                order "10em"))
        (if (== (db-toolbar-current-direction) "ascend")
            ((balloon (icon "tm_similar_previous.xpm") "Reverse ordering")
             (db-toolbar-direction "descend")))
        (if (== (db-toolbar-current-direction) "descend")
            ((balloon (icon "tm_similar_next.xpm") "Reverse ordering")
             (db-toolbar-direction "ascend")))
        // // //
        (text "Limit: ")
        (enum (when answer (db-toolbar-limit answer))
              (list "1" "2" "5" "10" "20" "50" "100" "200" "500" "1000" "")
              (db-toolbar-current-limit) "3em")
        >> >> >> >> >> >> >> >>
        (glue #t #f 0 24))))

(tm-define (load-db-buffer u)
  (load-buffer u)
  (db-show-toolbar))
