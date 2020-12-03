
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
;; Remote live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-remote-context? t)
  (and (live-context? t)
       (url-rooted-tmfs? (live-id t))))

(tm-define (live-url? fname)
  (string-starts? (url->string fname) "tmfs://live/"))

(tm-define (live-list-url? fname)
  (string-starts? (url->string fname) "tmfs://live-list/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-find-server lid)
  (let* ((s1 (url->string (url-unroot lid)))
         (s2 (if (== (tmfs-car s1) "live") (tmfs-cdr s1) s1))
         (sname (tmfs-car s2)))
    (client-find-server sname)))

(tm-define (live-get-name lid)
  (let* ((s1 (url->string (url-unroot lid)))
         (s2 (if (== (tmfs-car s1) "live") (tmfs-cdr s1) s1))
         (s3 (tmfs-cdr s2))
         (s4 (if (== (tmfs-car s3) "live") (tmfs-cdr s3) s3)))
    s4))

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
  (live-remote-retrieve (live-id t) (live-view-id t)))

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
               (when (debug-get "live")
                 (display* "Send " (patch->modlist p)
                           ", " old-state ", " new-state "\n"))
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
      (when (debug-get "live")
        (display* "Resend " (patch->modlist p)
                  ", " old-state ", " new-state "\n"))
      (live-remote-modify lid p old-state new-state))))

(tm-call-back (live-modify lid mods old-state new-state)
  (when (debug-get "live")
    (display* "Receive " mods ", " old-state ", " new-state "\n")
    ;;(display* "live-modify " lid
    ;;          ", " mods ", " old-state ", " new-state "\n")
    )
  (live-treat-pending lid)
  (with (server msg-id) envelope
    (let* ((old-t (live-get-document lid old-state)) ;; TODO: check old-t != #f
           (p (modlist->patch mods old-t))
           (inv-p (patch-invert p old-t))
           (ok-state (live-latest-compatible lid old-state p)))
      (when (debug-get "live")
        (when (!= ok-state (live-current-state lid))
          (display* "-- full-history= " (live-get-history lid) "\n")
          (display* "-- history= " (live-get-state-list lid old-state) "\n")
          (display* "-- ok-state= " ok-state "\n")
          (display* "-- mods= " mods "\n")
          (display* "-- oldp= "
                    (patch->modlist
                     (live-get-inverse-patch lid old-state)) "\n")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (live-list name type)
  (in? type (list "read")))

(tmfs-load-handler (live-list sname)
  (let* ((u (string-append "tmfs://live-list/" sname))
         (base (string-append "tmfs://live/" sname))
         (server (client-find-server sname)))
    (client-remote-eval server `(remote-list-live)
      (lambda (l)
        (with hyp (lambda (c) `(hlink ,c ,(string-append base "/" c)))
          (with doc `(document (section* "Live documents") ,@(map hyp l))
            (buffer-set-body u doc)
            (buffer-pretend-saved u)
            (set-message "retrieved contents" "list of live documents"))))
      (lambda (err)
        (set-message err "list of live documents")))
    (set-message "loading..." "list of live documents")
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "generic"))
       (body (document "")))))

(tm-define (list-live server)
  (and-with sname (client-find-server-name server)
    (string-append "tmfs://live-list/" sname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live discussions as documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (live name type)
  ;; FIXME: to be improved...
  (in? type (list "read" "write")))

(tmfs-title-handler (live fname doc)
  (let* ((lid (string-append "tmfs://live/" fname))
         (name (live-get-name lid))
         (title (string-append "Live - " name)))
    title))

(define live-vid-table (make-ahash-table))

(define (get-live-vid lid)
  (or (ahash-ref live-vid-table lid)
      (with vid (create-unique-id)
        (ahash-set! live-vid-table lid vid)
        vid)))

(tmfs-load-handler (live fname)
  (with lid (string-append "tmfs://live/" fname)
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "generic" "live-document"))
       (body (document
               (live-io* ,(get-live-vid lid) ,lid (document "")))))))

(tm-define (live-create-interactive server)
  (:interactive #t)
  (and-with sname (client-find-server-name server)
    (interactive
        (lambda (name)
          (with lid (string-append "tmfs://live/" sname "/" name)
            (client-remote-eval server `(live-exists? ,lid)
              (lambda (e?)
                (if (not e?) (load-document lid)
                    (set-message "live document already exists"
                                 "create live document"))))))
      (list "Create live document" "string" '()))))

(tm-define (live-open-interactive server)
  (:interactive #t)
  (and-with sname (client-find-server-name server)
    (interactive
        (lambda (name)
          (with lid (string-append "tmfs://live/" sname "/" name)
            (client-remote-eval server `(live-exists? ,lid)
              (lambda (e?)
                (if e? (load-document lid)
                    (set-message "live document not found"
                                 "open live document"))))))
      (list "Open live document" "string" '()))))
