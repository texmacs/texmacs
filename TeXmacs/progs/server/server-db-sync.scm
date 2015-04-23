
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-sync.scm
;; DESCRIPTION : synchronizing server databases with the client
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-db-sync)
  (:use (server server-tmfs)
        (database db-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determining changes in a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-db-changes kinds t)
  ;;(display* "remote-db-changes " kinds ", " t "\n")
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (with l (map (cut db-change-list uid <> t) kinds)
          (server-return envelope (list l (current-time)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying remote changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-remote-sync-one uid line)
  (cond ((== (cadr line) "remote-delete")
         (with (name cmd kind) line
           (with-database (user-database kind)
             (with-time :now
               (let* ((types (smart-ref db-kind-table kind))
                      ;; NOTE: db-kind-table should have been initialized
                      (ids (db-search `(("name" ,name)
                                        ("type" ,@types)
                                        ("owner" ,uid)))))
                 (for-each db-remove-entry ids))))))
        ((== (cadr cmd) "upload")
         (with (name cmd kind id val) line
           (for (attr '("owner" "readable" "writable"))
             (set! val (assoc-remove! val attr)))
           (set! val (assoc-set! val "owner" uid))
           (set! val (assoc-set! val "readable" "all"))
           (with-database (user-database kind)
             (with-time :now
               (let* ((types (smart-ref db-kind-table kind))
                      ;; NOTE: db-kind-table should have been initialized
                      (ids (db-search `(("name" ,name)
                                        ("type" ,@types)
                                        ("owner" ,uid)))))
                 (if (null? ids)
                     (if (db-entry-exists? id)
                         (db-create-entry val)
                         (db-set-entry id val))
                     (db-update-entry (car ids) val id))))))))
  #t)

(tm-service (remote-db-sync l)
  (display* "remote-db-sync " l "\n")
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (with ok? (list-and (map (cut db-remote-sync-one uid <>) l))
          (server-return envelope (and ok? (current-time)))))))
