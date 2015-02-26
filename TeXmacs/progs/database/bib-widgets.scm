
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-widgets.scm
;; DESCRIPTION : widgets for managing bibliographic databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-widgets)
  (:use (database bib-db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default bibliographic database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-bib-db)
  (url->system (string->url "$TEXMACS_HOME_PATH/database/bib.tmdb")))

(define-preferences
  ("bib-db" (default-bib-db) noop))

(define (get-bib-db)
  (get-preference "bib-db"))

(define (set-bib-db val)
  (set-preference "bib-db" val)
  (refresh-now "bib-db-preference"))

(define (get-bib-db-short)
  (with full (system->url (get-bib-db))
    (url->system (url-tail full))))

(define (set-bib-db-short val)
  (with full (system->url (get-bib-db))
    (set-bib-db (url->system (url-relative full (system->url val))))))

(tm-define (url-bib-db)
  (system->url (get-bib-db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (bib-preferences-widget)
  ===
  (padded
    (hlist
      (text "Bibliographic database: ") //
      (refreshable "bib-db-preference"
        (input (set-bib-db-short answer)
               "bib-db" (list (get-bib-db-short)) "15em"))))
  ===)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-bib-preferences)
  (:interactive #t)
  (top-window bib-preferences-widget "Bibliographic preferences"))
