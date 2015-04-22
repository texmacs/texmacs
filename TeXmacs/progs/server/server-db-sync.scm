
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
  (:use (server server-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determining changes in a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-change-list* uid kind t)
  (when (number? t) (set! t (number->string t)))
  (let* ((types (or (smart-ref db-kind-table kind) #t))
         ;; FIXME: db-kind-table may not be fully initialized
         (ids '()))
    (with-time :always
      (with-user #t
        (set! ids (db-search `(,@(if (== uid #t) (list)
                                     `(("owner" ,uid)))
                               ,@(if (== types #t) (list)
                                     `(("type" ,@types)))
                               (:modified ,t "10675199165"))))))
    (with-user #t
      (with get (lambda (id)
                  (list id
                        (with-time :always
                          (db-get-field-first id "name" #f))
                        (with-time :now
                          (db-get-entry id))))
        (map get ids)))))

(tm-service (remote-db-changes kind t)
  (display* "remote-db-changes " kind ", " t "\n")
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (with l (db-change-list* uid kind t)
          (server-return envelope l)))))
