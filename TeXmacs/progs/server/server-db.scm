
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-db.scm
;; DESCRIPTION : Remote databases, server side
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-db)
  (:use (server server-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change the time if needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (with-identifier-context id . body)
  `(if (npair? ,id)
       (begin
         ,@body)
       (with-global db-time (cadr ,id)
         (with-global ,id (car ,id)
           ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for basic database API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-get-field id attr)
  ;;(display* "remote-get-field " id ", " attr "\n")
  (with-identifier-context id
    (with uid (or (server-get-user envelope) "all")
      (cond ((not (db-allow? id uid "readable"))
             (server-error envelope "Error: read access required for field"))
            (else
              (server-return envelope (db-get-field id attr)))))))

(tm-service (remote-set-field id attr vals)
  ;;(display* "remote-set-field " id ", " attr ", " vals "\n")
  (with-identifier-context id
    (with uid (server-get-user envelope)
      (cond ((not uid)
             (server-error envelope "Error: not logged in"))
            ((not (db-allow? id uid "writable"))
             (server-error envelope "Error: write access required for field"))
            ((!= db-time :now)
             (server-error envelope "Error: cannot rewrite history"))
            (else
              (db-set-field id attr vals)
              (server-return envelope #t))))))

(tm-service (remote-get-attributes id)
  ;;(display* "remote-get-attributes " id "\n")
  (with-identifier-context id
    (with uid (or (server-get-user envelope) "all")
      (cond ((not (db-allow? id uid "readable"))
             (server-error envelope "Error: read access required for entry"))
            (else
              (server-return envelope (db-get-attributes id)))))))

(tm-service (remote-get-entry id)
  ;;(display* "remote-get-entry " id "\n")
  (with-identifier-context id
    (with uid (or (server-get-user envelope) "all")
      (cond ((not (db-allow? id uid "readable"))
             (server-error envelope "Error: read access required for entry"))
            (else
              (server-return envelope (db-get-entry id)))))))

(tm-service (remote-set-entry id l)
  ;;(display* "remote-set-entry " id ", " l "\n")
  (with-identifier-context id
    (with uid (server-get-user envelope)
      (cond ((not uid)
             (server-error envelope "Error: not logged in"))
            ((not (db-allow? id uid "writable"))
             (server-error envelope "Error: write access required for entry"))
            ((!= db-time :now)
             (server-error envelope "Error: cannot rewrite history"))
            (else
              (db-set-entry id l)
              (server-return envelope #t))))))

(tm-service (remote-create-entry l)
  ;;(display* "remote-create-entry " l "\n")
  (with uid (server-get-user envelope)
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          (else
            (with-user uid
              (server-return envelope (db-create-entry l)))))))

(tm-service (remote-search q)
  ;;(display* "remote-search " q "\n")
  (with uid (or (server-get-user envelope) "all")
    (with-user uid
      (server-return envelope (db-search q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get limited information about other users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-search-user q)
  ;;(display* "remote-search-user " q "\n")
  (with-user #t
    ;; TODO: in the future, we should only allow searches by name or by pseudo
    (server-return envelope (db-search (rcons q (list "type" "user"))))))

(define (get-user-field uid attr)
  (cond ((string? uid)
         (with-user #t
           (with r (db-get-field uid attr)
             (and (pair? r) (car r)))))
        ((list? uid)
         (map (cut get-user-field <> attr) uid))
        (else #f)))

(tm-service (remote-get-user-pseudo uid)
  ;;(display* "remote-search-user-pseudo " uid "\n")
  (server-return envelope (get-user-field uid "pseudo")))

(tm-service (remote-get-user-name uid)
  ;;(display* "remote-search-user-name " uid "\n")
  (server-return envelope (get-user-field uid "name")))
