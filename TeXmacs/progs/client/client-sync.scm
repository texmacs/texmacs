
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-sync.scm
;; DESCRIPTION : synchronizing client files with the server
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-sync)
  (:use (client client-db-sync)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build list of files to be synchronized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dont-sync? u)
  (with last (url->string (url-tail u))
    (or (string-starts? last ".")
        (string-starts? last "#")
        (string-starts? last "svn-")
        (string-ends? last "~")
        (string-ends? last ".aux")
        (string-ends? last ".bak")
        (string-ends? last ".bbl")
        (string-ends? last ".blg")
        (string-ends? last ".log")
        (string-ends? last ".tmp"))))

(tm-define (client-sync-list u)
  (set! u (url->url u))
  (cond ((dont-sync? u) (list))
        ((not (url-exists? u)) (list))
        ((not (url-directory? u)) (list (list #f u)))
        (else
          (let* ((dirl (url-append u (url-wildcard "*")))
                 (l (url->list (url-expand (url-complete dirl "r")))))
            (cons (list #t u)
                  (append-map client-sync-list l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Merge with list of remote files to be synchronized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (url-subtract u base)
  (if (== (url->url u) (url->url base))
      (string->url ".")
      (url-delta (url-append base "dummy") u)))

(define (first-string-leq? l1 l2)
  (and (pair? l1) (pair? l2)
       (string<=? (car l1) (car l2))))

(define (compute-sync-list local-base remote-base cont)
  ;;(display* "compute-sync-list " local-base ", " remote-base "\n")
  (let* ((rbase (remote-file-name remote-base))
         (server-name (tmfs-car rbase))
         (server (client-find-server server-name))
         (local-l (client-sync-list local-base))
         (t (make-ahash-table)))
    ;;(for (x local-l)
    ;;  (display* "Got local " x "\n"))
    (client-remote-eval server `(remote-sync-list ,rbase)
      (lambda (remote-l)
        ;;(for (x remote-l)
	;;  (display* "Got remote " x "\n"))
        (for (local-e local-l)
          (with (dir? name) local-e
            (with d (url->string (url-subtract name local-base))
              (ahash-set! t d (list dir? #t #f)))))
        (for (remote-e remote-l)
          (with (dir? name id) remote-e
            (let* ((base (tmfs-cdr (remote-file-name remote-base)))
                   (d (url->string (url-subtract name base)))
                   (prev (ahash-ref t d)))
              (if (not prev)
                  (ahash-set! t d (list dir? #f id))
                  (ahash-set! t d (list dir? (cadr prev) id))))))
        (with l (sort (ahash-table->list t) first-string-leq?)
          ;;(for (x l)
	  ;;  (display* "Intermediate: " x "\n"))
          (cont l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determine local files which have to be uploaded
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-url-sync-info dir? local-exists? local-name remote-name)
  (with-database* (user-database "sync")
    (let* ((ids (db-search `(("name" ,(url->system local-name))
                             ("remote-name" ,(url->system remote-name))
                             ("type" "sync"))))
           (date (and local-exists?
                      (number->string (url-last-modified local-name)))))
      (if (nnull? ids)
          (let* ((local-id (car ids))
                 (date* (db-get-field-first local-id "date" #f))
                 (remote-id* (db-get-field-first local-id "remote-id" #f)))
            (list dir? local-name local-id date date* remote-id*))
          (with local-id
              (db-create-entry `(("type" "sync")
                                 ("name" ,(url->system local-name))
                                 ("remote-name" ,(url->system remote-name))
                                 ,@(if date `(("date" ,date)) `())))
            (list dir? local-name local-id date #f #f))))))

(define (get-sync-status info remote-name remote-id)
  (with (dir? local-name local-id date date* remote-id*) info
    ;;(display* "Sync status: " dir? ", " local-name ", " local-id
    ;;          "; " date* " -> " date
    ;;          "; " remote-name ", " remote-id* " -> " remote-id "\n")
    (let* ((local-info (list (url->system local-name) local-id))
           (remote-info (list (url->system remote-name) remote-id))
           (all-info (append (list dir?) local-info remote-info)))
    (cond ((and date remote-id (== date date*) (== remote-id remote-id*)) #f)
          ((and dir? date date* remote-id remote-id*) #f)
          ((and date (== date date*) remote-id* (not remote-id))
           (cons "local-delete" all-info))
          ((and date* (not date) remote-id (== remote-id remote-id*))
           (cons "remote-delete" all-info))
          ((and (!= remote-id remote-id*) (== date date*))
           (cons "download" all-info))
          ((and (!= date date*) (== remote-id remote-id*))
           (cons "upload" all-info))
          ((and remote-id (not date) (not date*))
           (cons "download" all-info))
          ((and date (not remote-id) (not remote-id*))
           (cons "upload" all-info))
          (else
            (let* ((suf1 (if date "*" "-"))
                   (suf2 (if remote-id "*" "-")))
              (cons (string-append "conflict" suf1 suf2) all-info)))))))

(define (url-append* base u)
  (if (== (url->url u) (string->url ".")) base (url-append base u)))

(define (prepend-file-dir dir? name)
  (cond ((not name) u)
        (dir? (string->url (string-append "tmfs://remote-dir/" name)))
        (else (string->url (string-append "tmfs://remote-file/" name)))))

(define (file-dir-correct dir? u)
  (prepend-file-dir dir? (remote-file-name u)))

(define (conflicting-local-delete? line ref)
  (and (!= (car line) "local-delete")
       (third line)
       (url-descends? (system->url (third line))
                      (system->url (third ref)))))

(define (conflicting-remote-delete? line ref)
  (and (!= (car line) "remote-delete")
       (fifth line)
       (url-descends? (system->url (fifth line))
                      (system->url (fifth ref)))))

(define (requalify-deleted line l)
  (with (cmd dir? local-name local-id remote-name remote-id*) line
    (cond ((and (== cmd "local-delete")
                (list-or (map (cut conflicting-local-delete? <> line) l)))
           ;;(display* "Requalify deleted " line "\n")
           (cons "conflict*-" (cdr line)))
          ((and (== cmd "remote-delete")
                (list-or (map (cut conflicting-remote-delete? <> line) l)))
           ;;(display* "Requalify deleted " line "\n")
           (cons "conflict-*" (cdr line)))
          (else line))))

(tm-define (requalify-conflicting line t)
  (with (cmd dir? local-name local-id remote-name remote-id*) line
    (let* ((action (or (ahash-ref t local-name) "Keep"))
           (n (string-length cmd)))
      (cond ((not (string-starts? cmd "conflict")) line)
            ((== action "Keep") line)
            ((and (== action "Local")
                  (== (substring cmd (- n 2) (- n 1)) "*"))
             (cons "upload" (cdr line)))
            ((and (== action "Local")
                  (== (substring cmd (- n 2) (- n 1)) "-"))
             (cons "remote-delete" (cdr line)))
            ((and (== action "Remote")
                  (== (substring cmd (- n 1) n) "*"))
             (cons "download" (cdr line)))
            ((and (== action "Remote")
                  (== (substring cmd (- n 1) n) "-"))
             (cons "local-delete" (cdr line)))
            (else line)))))

(tm-define (client-sync-status local-base remote-base cont)
  ;;(display* "client-sync-status " local-base ", " remote-base "\n")
  (compute-sync-list local-base remote-base
    (lambda (l)
      (with r (list)
        (for (x l)
          (with (rname dir? local? remote-id) x
            (let* ((local-name (url-append* local-base rname))
                   (remote-name-pre (url-append* remote-base rname))
                   (remote-name (file-dir-correct dir? remote-name-pre))
                   (info (get-url-sync-info dir? local? local-name remote-name))
                   (next (get-sync-status info remote-name remote-id)))
              (when next (set! r (cons next r))))))
        (set! r (map (cut requalify-deleted <> r) r))
        (cont (reverse r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for testing and repairing broken entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (sync-test)
  (client-sync-status (string->url "~/test/sync-test") (current-buffer)
    (lambda (l)
      (display* "----- result -----\n")
      (for (x l)
        (display* x "\n")))))

(tm-define (sync-repair-one id)
  (and-with lname (db-get-field-first id "name" #f)
    (when (not (url-exists? lname))
      (display* "Problematic entry: " id ", " lname "\n")
      (db-remove-entry id))))

(tm-define (sync-repair)
  (with-database* (user-database "sync")
    (with ids (db-search `(("type" "sync")))
      (for-each sync-repair-one ids))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transmitting the bulk data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (filter-status-list l which)
  (list-filter l (lambda (line) (string-starts? (car line) which))))

(define (status-line->server line)
  (with sname (tmfs-car (remote-file-name (fifth line)))
    (client-find-server sname)))

(define (append-doc line)
  (with (cmd dir? local-name local-id remote-name remote-id*) line
    (with doc (if dir? "" (string-load (system->url local-name)))
      (rcons line doc))))

(define (post-upload line uploaded)
  ;;(display* "post-upload " line ", " uploaded "\n")
  (and uploaded
       (with-database* (user-database "sync")
         (with (cmd dir? local-name local-id remote-name remote-id* doc) line
           (with remote-id uploaded
             (let* ((u (system->url local-name))
                    (date (number->string (url-last-modified u)))
                    (sync-date (number->string (current-time))))
               (db-set-field local-id "remote-id" (list remote-id))
               (db-set-field local-id "date" (list date))
               (db-set-field local-id "sync-date" (list sync-date))
               #t))))))

(define (client-upload uploads* msg cont)
  (if (null? uploads*) (cont #t)
      (with uploads (map append-doc uploads*)
        (with server (status-line->server (car uploads))
          (client-remote-eval server `(remote-upload ,uploads ,msg)
            (lambda (r)
              (when (and (list? r) (== (length r) (length uploads)))
                (with success? (list-and (map post-upload uploads r))
                  (cont success?)))))))))

(define (post-download line downloaded)
  ;;(display* "post-download " line ", " downloaded "\n")
  (and downloaded
       (with-database* (user-database "sync")
         (with (cmd dir? local-name local-id remote-name remote-id*) line
           (with (remote-id doc) downloaded
             (with u (system->url local-name)
               (if dir?
                   (when (not (url-exists? u))
                     (system-mkdir u))
                   (string-save doc u))
               (let* ((date (number->string (url-last-modified u)))
                      (sync-date (number->string (current-time))))
                 (db-set-field local-id "remote-id" (list remote-id))
                 (db-set-field local-id "date" (list date))
                 (db-set-field local-id "sync-date" (list sync-date))
                 #t)))))))

(define (client-download downloads cont)
  (if (null? downloads) (cont #t)
      (with server (status-line->server (car downloads))
        (client-remote-eval server `(remote-download ,downloads)
          (lambda (r)
            (when (and (list? r) (== (length r) (length downloads)))
              (with success? (list-and (map post-download downloads r))
                (cont success?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing files and directories which disappeared on the other side
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-local-one line)
  ;;(display* "remove-local-one " line "\n")
  (with-database* (user-database "sync")
    (with (cmd dir? local-name local-id remote-name remote-id*) line
      (with u (system->url local-name)
        (if dir? (system-rmdir u) (system-remove u)))
      (db-remove-entry local-id))))

(define (remove-local remove-l cont)
  (set! remove-l (reverse remove-l))
  (for (line remove-l)
    (remove-local-one line))
  (cont #t))

(define (post-remove-remote line remove-status)
  ;;(display* "post-remove-remote " line ", " remove-status "\n")
  (and remove-status
       (with-database* (user-database "sync")
         (with (cmd dir? local-name local-id remote-name remote-id*) line
           (db-remove-entry local-id)))))

(define (remove-remote remove-l cont)
  (set! remove-l (reverse remove-l))
  (if (null? remove-l) (cont #t)
      (with server (status-line->server (car remove-l))
        (client-remote-eval server `(remote-remove-several ,remove-l)
          (lambda (r)
            (when (and (list? r) (== (length r) (length remove-l)))
              (with success? (list-and (map post-remove-remote remove-l r))
                (cont success?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-upload local-name remote-name msg . cont)
  (client-sync-status local-name remote-name
    (lambda (l)
      (client-upload (append (filter-status-list l "upload")
			     (filter-status-list l "conflict"))
		     msg (if (null? cont) ignore (car cont))))))

(tm-define (remote-download local-name remote-name . cont)
  (client-sync-status local-name remote-name
    (lambda (l)
      (client-download (append (filter-status-list l "download")
                               (filter-status-list l "conflict"))
                       (if (null? cont) ignore (car cont))))))

(tm-define (client-sync-proceed l msg cont)
  (client-upload (filter-status-list l "upload") msg
    (lambda (upload-ok?)
      ;;(display* "Uploading done " upload-ok? "\n")
      (client-download (filter-status-list l "download")
        (lambda (download-ok?)
          ;;(display* "Downloading done " download-ok? "\n")
          (remove-remote (filter-status-list l "remote-delete")
            (lambda (remote-delete-ok?)
              ;;(display* "Remote deletions done " remote-delete-ok? "\n")
              (remove-local (filter-status-list l "local-delete")
                (lambda (local-delete-ok?)
                  ;;(display* "Local deletions done " local-delete-ok? "\n")
                  (cont))))))))))

(tm-define (client-auto-sync-list server)
  (with-database* (user-database "sync")
    (let* ((server-name (client-find-server-name server))
           (ids (db-search `(("type" "auto-sync")
                             ("server" ,server-name)))))
      (with build (lambda (id)
                    (let* ((lname (db-get-field-first id "name" ""))
                           (rname (db-get-field-first id "remote-name" "")))
                      (cons lname rname)))
        (map build ids)))))

(tm-define (client-auto-sync-add server lname rname)
  (with-database* (user-database "sync")
    (let* ((server-name (client-find-server-name server)))
      (client-auto-sync-remove server lname)
      (db-create-entry `(("type" "auto-sync")
                         ("server" ,server-name)
                         ("name" ,lname)
                         ("remote-name" ,rname))))))

(tm-define (client-auto-sync-remove server lname)
  (with-database* (user-database "sync")
    (let* ((server-name (client-find-server-name server))
           (ids (db-search `(("type" "auto-sync")
                             ("server" ,server-name)
                             ("name" ,lname)))))
      (for (id ids)
        (db-remove-entry id)))))
