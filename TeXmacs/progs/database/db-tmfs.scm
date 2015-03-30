
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-tmfs.scm
;; DESCRIPTION : databases as documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-tmfs)
  (:use (database db-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-style kind)
  (string-append "database-" kind))

(tmfs-title-handler (db name doc)
  (with kind name ;; (tmfs-car name)
    (string-append "Database - " kind)))

(define (get-db-fields kind)
  (with types (or (smart-ref db-kind-table kind) (list))
    (with-database (user-database)
      (cdr (db-load-types types)))))

(tmfs-load-handler (db name)
  (let* ((kind name) ;; (tmfs-car name)
         (l (get-db-fields name))
         (l* (if (null? l) (list "") l)))
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple ,(db-get-style kind)))
       (body (document ,@l*)))))
