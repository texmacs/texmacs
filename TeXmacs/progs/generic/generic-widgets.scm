
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-widgets.scm
;; DESCRIPTION : widgets for general purpose editing
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-widgets)
  (:use (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting the search results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (inside-search-widget?)
  (== (current-buffer) (string->url "tmfs://aux/search")))

(tm-define (keyboard-press key time)
  (:require (inside-search-widget?))
  (former key time)
  (display* "key= " key " in " (buffer-get-master (current-buffer)) "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((search-widget u style aux) quit)
  (padded
    (resize "600px" "60px"
      (texmacs-input `(document "") `(style (tuple ,@style)) aux))
    ======
    (explicit-buttons
      (hlist
        ("Previous" (noop)) // //
        ("Next" (noop)) >>>
        ("Done" (quit))))))

(tm-define (open-search)
  (:interactive #t)
  (let* ((u  (current-buffer))
         (st (list-remove-duplicates (rcons (get-style-list) "macro-editor")))
         (aux "tmfs://aux/search"))
    (buffer-set-master aux u)
    (dialogue-window (search-widget u st aux) noop "Search")))
