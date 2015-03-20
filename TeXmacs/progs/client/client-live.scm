
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
  (:use (utils relate live-connection)
        (utils relate live-view)
        (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags for remote live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-remote-context? t)
  (and (tree-func? t 'live-io 3)
       (live-context? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-find-server lid)
  (with sname (tmfs-car (url->string (url-unroot lid)))
    (client-find-server sname)))

(tm-define (live-get-name lid)
  (with rname (tmfs-cdr (url->string (url-unroot lid)))
    (if (== (tmfs-car rname) "live") (tmfs-cdr rname) rname)))

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
    (let* ((sname (tmfs-car (url->string (url-unroot lid))))
           (server (live-find-server lid)))
      (if server
          (client-remote-eval server `(live-open ,lid)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treating local modifications in live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define following-server-instruction? #f)

(define (live-remote-modify lid p old-state new-state)
  (let* ((server (live-find-server lid))
         (mods (patch->modlist p))
         (cmd `(live-modify ,lid ,mods ,old-state ,new-state)))
    (client-remote-eval server cmd
      (lambda (ok?)
        (when ok?
          (live-set-remote-state lid server new-state)
          (live-forget-obsolete lid)))
      (lambda (err)
        (display* "TeXmacs] " err "\n")
        (set-message err "modify remote live document")))))

(tm-define (live-apply-patch lid p . opt-state)
  (let* ((old-state (live-current-state lid))
         (new-state (apply former (cons* lid p opt-state)))
         (server (live-find-server lid)))
    (if (or (not server) following-server-instruction?) new-state
        (and old-state new-state
             (begin
	       (display* "Send " (patch->modlist p)
			 ", " old-state ", " new-state "\n")
               (live-remote-modify lid p old-state new-state)
               new-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling modifications send by the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-pull-list p l)
  (if (null? l) (list p l)
      (with (p* l*) (patch-pull-list p (cdr l))
        (list (patch-pull p* (car l))
              (cons (patch-co-pull p* (car l)) l*)))))

(define (live-resend-local-changes lid old-state)
  (let* ((new-state (live-current-state lid))
         (p (live-get-inverse-patch lid old-state)))
    (when (!= new-state old-state)
      (display* "Resend " (patch->modlist p)
		", " old-state ", " new-state "\n")
      (live-remote-modify lid p old-state new-state))))

(tm-call-back (live-modify lid mods old-state new-state)
  (display* "Receive " mods ", " old-state ", " new-state "\n")
  ;;(display* "live-modify " lid ", " mods ", " old-state ", " new-state "\n")
  (live-treat-pending lid)
  (with (server msg-id) envelope
    (let* ((old-t (live-get-document lid old-state)) ;; TODO: check old-t != #f
           (p (modlist->patch mods old-t))
           (inv-p (patch-invert p old-t))
           (ok-state (live-latest-compatible lid old-state p)))
      (when (!= ok-state (live-current-state lid))
	(display* "-- full-history= " (live-get-history lid) "\n")
	(display* "-- history= " (live-get-state-list lid old-state) "\n")
	(display* "-- ok-state= " ok-state "\n")
	(display* "-- mods= " mods "\n")
	(display* "-- oldp= "
		  (patch->modlist
		   (live-get-inverse-patch lid old-state)) "\n"))
      (live-retract lid ok-state)
      (live-update-views lid)
      (let* ((rev (live-get-patch-list lid old-state))
             (p* (patch-pull p (patch-compound rev)))
             (rev* (cadr (patch-pull-list p rev)))
             (states (live-get-state-list lid old-state))
             (states* (map (lambda (x) (create-unique-id)) states))
             (new-states (rcons states* new-state))
             (new-changes (rcons rev* inv-p)))
	(with-global following-server-instruction? #t
	  (live-apply-patch lid p* (car new-states)))
        (live-update-views lid)
        (live-rewrite-history lid old-state new-states new-changes)
        (live-set-remote-state lid server new-state)
        (with result (client-return envelope #t)
          (live-resend-local-changes lid new-state)
          result)))))
