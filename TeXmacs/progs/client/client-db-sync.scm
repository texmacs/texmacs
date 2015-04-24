
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-sync.scm
;; DESCRIPTION : synchronizing client databases with the server
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-db-sync)
  (:use (client client-tmfs)
        (database db-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Information about last synchronization and auto-sync kinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-sync-kinds server)
  (list-filter (list "bib")
	       (cut db-sync-kind? server <>)))

(tm-define (db-sync-kind server kind on?)
  (with-database (user-database "sync")
    (let* ((server-name (client-find-server-name server))
           (ids (db-search `(("type" "db-sync-kind")
                             ("server" ,server-name)
			     ("kind" ,kind))))
           (id (if (nnull? ids) (car ids) (db-create-id))))
      (db-set-entry id `(("type" "db-sync-kind")
                         ("server" ,server-name)
                         ("kind" ,kind)
                         ("enabled" ,(if on? "yes" "no")))))))

(tm-define (db-sync-kind? server kind)
  (with-database (user-database "sync")
    (let* ((server-name (client-find-server-name server))
           (ids (db-search `(("type" "db-sync-kind")
                             ("server" ,server-name)
			     ("kind" ,kind)))))
      (or (null? ids)
	  (!= (db-get-field-first (car ids) "enabled" "yes") "no")))))

(define (db-last-sync server)
  (with-database (user-database "sync")
    (let* ((server-name (client-find-server-name server))
           (ids (db-search `(("type" "db-sync")
                             ("server" ,server-name)))))
      (if (null? ids)
          (list "0" "0")
          (list (db-get-field-first (car ids) "local-sync" "0")
                (db-get-field-first (car ids) "remote-sync" "0"))))))

(define (db-dub-in-sync server ltime rtime)
  ;;(display* "db-dub-in-sync " (client-find-server-name server)
  ;;          ", " ltime ", " rtime "\n")
  (with-database (user-database "sync")
    (let* ((server-name (client-find-server-name server))
           (ids (db-search `(("type" "db-sync")
                             ("server" ,server-name))))
           (id (if (nnull? ids) (car ids) (db-create-id))))
      (db-set-entry id `(("type" "db-sync")
                         ("name" "last-sync")
                         ("server" ,server-name)
                         ("local-sync" ,(number->string (+ ltime 1)))
                         ("remote-sync" ,(number->string (+ rtime 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building status list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-change-table l)
  (with t (make-ahash-table)
    (for (x l)
      (with (id name val) x
        (when (and name (or (not (ahash-ref t name)) (nnull? val)))
          (ahash-set! t name (list id val)))))
    t))

(define (db-equivalent? e1 e2)
  (for (x (cons* "owner" "readable" "writable" (db-meta-attributes)))
    (set! e1 (assoc-remove! e1 x))
    (set! e2 (assoc-remove! e2 x)))
  (with leq? (lambda (f1 f2) (string<=? (car f1) (car f2)))
    (== (sort e1 leq?) (sort e2 leq?))))

(define (db-change-status local-l remote-l kind)
  (let* ((local-t (db-change-table local-l))
         (remote-t (db-change-table remote-l))
         (status-t (make-ahash-table))
         (local-names (map car (ahash-table->list local-t)))
         (remote-names (map car (ahash-table->list remote-t)))
         (names (list-union local-names remote-names)))
    (for (name names)
      (let* ((local-pair (ahash-ref local-t name))
             (remote-pair (ahash-ref remote-t name))
             (local-id (and local-pair (car local-pair)))
             (remote-id (and remote-pair (car remote-pair)))
             (local-val (and local-pair (cadr local-pair)))
             (remote-val (and remote-pair (cadr remote-pair))))
        (cond ((and (null? local-val) (not remote-val))
               (ahash-set! status-t name
                           (list "remote-delete" kind)))
              ((and (null? remote-val) (not local-val))
               (ahash-set! status-t name
                           (list "local-delete" kind)))
              ((and local-val (not remote-val))
               (ahash-set! status-t name
                           (list "upload" kind local-id local-val)))
              ((and remote-val (not local-val))
               (ahash-set! status-t name
                           (list "download" kind remote-id remote-val)))
              ((and local-val remote-val (db-equivalent? local-val remote-val))
               (noop))
              (else
               (ahash-set! status-t name
                           (list "conflict" kind
                                 local-id remote-id
                                 local-val remote-val))))))
    (with leq? (lambda (f1 f2) (string<=? (car f1) (car f2)))
      (sort (ahash-table->list status-t) leq?))))

(tm-define (db-filter-status-list dbl cmd)
  (list-filter dbl (lambda (line) (== (cadr line) cmd))))

(define (db-list-item line)
  (with (name cmd kind . args) line
    (string-append kind " - " name)))

(tm-define (db-requalify-conflicting line t)
  (if (!= (cadr line) "conflict") line
      (with (name cmd kind local-id remote-id local-val remote-val) line
        (let* ((key (string-append kind " - " name))
               (action (or (ahash-ref t key) "Remote")))
          (cond ((and (== action "Local") (null? local-val))
                 (list name "remote-delete" kind))
                ((and (== action "Local") (nnull? local-val))
                 (list name "upload" kind local-id local-val))
                ((and (== action "Remote") (null? remote-val))
                 (list name "local-delete" kind))
                ((and (== action "Remote") (nnull? remote-val))
                 (list name "download" kind remote-id remote-val))
                (else line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying local changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-local-sync-one line)
  (cond ((== (cadr line) "local-delete")
         (with (name cmd kind) line
           (with-database (user-database kind)
             (with-time :now
               (with ids (db-search `(("name" ,name)))
                 ;;(display* ">>> At " (current-time)
                 ;;          ", remove " ids "\n")
                 (for-each db-remove-entry ids))))))
        ((== (cadr line) "download")
         (with (name cmd kind id val) line
           (for (attr '("owner" "readable" "writable"))
             (set! val (assoc-remove! val attr)))
           (with-database (user-database kind)
             (with-time :now
               (with ids (db-search `(("name" ,name)))
                 (if (null? ids)
                     (if (db-entry-exists? id)
                         (begin
                           ;;(display* ">>> At " (current-time)
                           ;;          ", create " val "\n")
                           (db-create-entry val))
                         (begin
                           ;;(display* ">>> At " (current-time)
                           ;;          ", set " id " := " val "\n")
                           (db-set-entry id val)))
                     (begin
                       ;;(display* ">>> At " (current-time)
                       ;;          ", update " (car ids) " := " val
                       ;;          ", suggest " id "\n")
                       (db-update-entry (car ids) val id)))))))))
  #t)

(define (db-local-sync l kinds ltime)
  (and (null? (apply append (car (local-db-changes kinds ltime))))
       (list-and (map db-local-sync-one l))
       (current-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-server)
  (with l (client-active-servers)
    (and (nnull? l) (car l))))

(tm-define (db-sync-test)
  (db-client-sync-status (current-server)
    (lambda (status-l ltime rtime)
      (for (x status-l)
        (display* "Status: " x "\n"))
      (display* "ltime : " ltime "\n")
      (display* "rtime : " rtime "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (local-db-changes kinds t)
  ;;(display* "local-db-changes " kinds ", " t "\n")
  (with l (map (lambda (kind)
                 (with-database (user-database kind)
                   (db-change-list #t #t t)))
               kinds)
    (list l (current-time))))

(tm-define (db-client-sync-status server cont)
  (let* ((kinds (db-sync-kinds server))
         (timep (db-last-sync server)))
    (with (ltime rtime) timep
      (with local-p (local-db-changes kinds ltime)
        (with (local-l ltime*) local-p
          ;;(for (ll local-l)
          ;;  (for (x ll)
          ;;    (display* "Local: " x "\n")))
          (client-remote-eval server `(remote-db-changes ,kinds ,rtime)
            (lambda (remote-p)
              (with (remote-l rtime*) remote-p
                ;;(for (rl remote-l)
                ;;  (for (x rl)
                ;;    (display* "Remote: " x "\n")))
                (with status-l (append-map db-change-status
                                           local-l remote-l kinds)
                  ;;(for (x status-l)
                  ;;  (display* "Status: " x "\n"))
                  (when (and (null? status-l)
                             (nnull? (apply append (append local-l remote-l))))
                    (db-dub-in-sync server ltime* rtime*))
                  (cont status-l ltime* rtime*))))))))))

(tm-define (db-client-sync-proceed server l ltime rtime cont)
  (with kinds (db-sync-kinds server)
    (if (nnull? (apply append (car (local-db-changes kinds ltime))))
        (cont #f)
        (client-remote-eval server `(remote-db-sync ,l ,kinds ,rtime)
          (lambda (rtime*)
            (if (not rtime*) (cont #f)
                (with ltime* (db-local-sync l kinds ltime)
                  (db-dub-in-sync server ltime* rtime*)
                  (cont #t))))))))
