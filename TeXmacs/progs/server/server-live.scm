
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-live.scm
;; DESCRIPTION : Live shared documents (server side)
;; COPYRIGHT   : (C) 2015-2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-live)
  (:use (utils relate live-connection)
        (server server-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storing live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (live-name lid)
  (let* ((s1 (url->string (url-unroot lid)))
         (s2 (if (== (tmfs-car s1) "live") (tmfs-cdr s1) s1))
         (s3 (tmfs-cdr s2))
         (s4 (if (== (tmfs-car s3) "live") (tmfs-cdr s3) s3)))
    s4))

(define (live-new uid lid)
  (with-time-stamp #t
    (with rid (db-create-entry `(("type" "live")
                                 ("name" ,(live-name lid))
                                 ("owner" ,uid)
                                 ("readable" "all")
                                 ("writable" "all")))
      (repository-add rid "tm")
      rid)))

(define (live-find lid)
  (with l (db-search `(("type" "live")
                       ("name" ,(live-name lid))))
    (and (nnull? l) (car l))))

(define (live-load lid)
  (and-let* ((rid (live-find lid))
             (fname (repository-get rid))
             (doc (if (url-exists? fname) (string-load fname) "")))
    (if (== doc "")
        `(document "")
        (convert doc "texmacs-snippet" "texmacs-stree"))))

(define (live-save lid t)
  (and-let* ((rid (live-find lid))
             (fname (repository-get rid))
             (doc (convert t "texmacs-stree" "texmacs-snippet")))
    (string-save doc fname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-waiting (make-ahash-table))

(define (live-applicable? lid client p old-state)
  (when (and (list? p) (== (live-current-state lid) old-state))
    (set! p (modlist->patch p (live-current-document lid))))
  (cond ((!= (live-get-remote-state lid client) old-state)
	 (display* "  ** Bad remote state " (live-get-remote-state lid client)
                   " instead of " old-state "\n"))
	((!= (live-current-state lid) old-state)
	 (display* "  ** Bad state " (live-current-state lid)
                   " instead of " old-state "\n"))
	((not (with doc (live-current-document lid)
		(patch-applicable? p doc)))
	 (display* "  ** Non applicable patch " (patch->scheme p) "\n")))
  (and (== (live-get-remote-state lid client) old-state)
       (== (live-current-state lid) old-state)
       (with doc (live-current-document lid)
         (patch-applicable? p doc))))

(define (live-apply lid client p old-state new-state)
  (when (and (list? p) (== (live-current-state lid) old-state))
    (set! p (modlist->patch p (live-current-document lid))))
  (and (== (live-current-state lid) old-state)
       (live-apply-patch lid p new-state)
       (begin
         (display* "Confirm " client ": " new-state "\n")
         (live-set-remote-state lid client new-state)
	 (live-forget-obsolete lid)
         (live-broadcast lid)
         new-state)))

(define (live-update lid client state)
  (with key (list lid client)
    (when (not (ahash-ref live-waiting key))
      (ahash-set! live-waiting key #t)
      (let* ((p (live-get-inverse-patch lid state))
             (mods (patch->modlist p))
             (new-state (live-current-state lid)))
	(display* "Send " client ": "
		  `(live-modify ,lid ,mods ,state ,new-state) "\n")
        (server-remote-eval client `(live-modify ,lid ,mods ,state ,new-state)
          (lambda (ok?)
	    (display* "Confirm " client ": " new-state ", " ok? "\n")
            (ahash-remove! live-waiting key)
            (when ok?
	      (live-set-remote-state lid client new-state)
	      (live-forget-obsolete lid))
            (live-broadcast-one lid client)))))))

(define (live-broadcast-one lid client)
  (with state (live-get-remote-state lid client)
    (when (and (!= state (live-current-state lid))
               (active-client? client))
      (live-update lid client state))))

(define (live-broadcast lid)
  (for (client (live-get-connections lid))
    (live-broadcast-one lid client)))

(tm-define (server-remove client)
  (former client)
  (for (key (map car (ahash-table->list live-waiting)))
    (when (== (cadr key) client)
      (ahash-remove! live-waiting key)))
  (for (lid (live-remote-connections client))
    (with-database (server-database)
      (live-save lid (tm->stree (live-current-document lid))))
    (live-hang-up lid client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (live-open lid)
  ;; Connect client to the live channel lid
  ;;(display* "live-open " lid "\n")
  (when (not (live-current-document lid))
    (when (not (live-find lid))
      (let* ((uid (server-get-user envelope))
             (rid (live-new uid lid))
             (doc '(document "")))
        (live-save lid doc)))
    (with doc (live-load lid)
      (live-create lid doc)))
  (with (client msg-id) envelope
    (live-connect lid client)
    (let* ((doc (live-current-document lid))
           (state (live-get-remote-state lid client)))
      (server-return envelope (list state (tm->stree doc))))))

(tm-service (live-modify lid mods old-state new-state)
  ;; States that the 'new-state' of the client is obtained
  ;; from 'old-state' by applying the list of modifications 'mods'
  (with (client msg-id) envelope
    (display* "Receive " client ": " mods ", " old-state ", " new-state "\n")
    (with ok? (live-applicable? lid client mods old-state)
      (when (not ok?)
	(display* ">> refuse " client ", " mods
		  ", state= " (live-current-state lid) "\n"))
      (when ok?
        (live-apply lid client mods old-state new-state))
      (server-return envelope ok?))))
