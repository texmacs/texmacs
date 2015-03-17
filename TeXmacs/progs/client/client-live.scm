
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-live.scm
;; DESCRIPTION : Live shared documents (client side)
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-live)
  (:use (utils relate live-view)
        (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags for remote live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-remote-context? t)
  (and (tree-func? t 'live-io 3)
       (live-context? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving the initial document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-remote-initializing (make-ahash-table))

(define (live-terminate-retrieval lid)
  (with t (ahash-ref live-remote-initializing lid)
    (when t
      (for (vid (ahash-set->list t))
        (live-view-set-state lid vid (live-current-state lid))))
    (live-restore-views lid)
    (ahash-remove! live-remote-initializing lid)))

(define (live-remote-retrieve lid vid)
  (when (ahash-ref live-remote-initializing lid)
    (ahash-set! (ahash-ref live-remote-initializing lid) vid #t))
  (when (not (ahash-ref live-remote-initializing lid))
    (ahash-set! live-remote-initializing lid (make-ahash-table))
    (ahash-set! (ahash-ref live-remote-initializing lid) vid #t)
    (let* ((fname (url->string (url-unroot lid)))
           (sname (tmfs-car fname))
           (server (client-find-server sname))
           (rname (tmfs-cdr fname))
           (lname (if (== (tmfs-car rname) "live") (tmfs-cdr rname) rname)))
      (if server
          (client-remote-eval server `(live-open ,lname)
            (lambda (msg)
              (with (state doc) msg
                (live-create lid doc state)
                (live-connect lid server)
                (live-terminate-retrieval lid)))
            (lambda (err)
              (display* "TeXmacs] " err "\n")
              (set-message err "retrieve remote live document")))
          (begin
            (display* "TeXmacs] could not connect to server " sname "\n")
            (set-message `(concat "could not connect to server " ,sname)
                         "retrieve remote live document"))))))

(tm-define (live-retrieve t)
  (:require (live-remote-context? t))
  (let* ((lid (live-id t))
         (vid (live-view-id t)))
    (if (url-rooted-tmfs? lid)
        (live-remote-retrieve lid vid)
        (former t))))
