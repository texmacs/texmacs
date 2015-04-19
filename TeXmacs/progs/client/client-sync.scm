
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
  (:use (client client-tmfs)))

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
  (if (== u base)
      (string->url ".")
      (url-delta (url-append base "dummy") u)))

(define (first-string-leq? l1 l2)
  (and (pair? l1) (pair? l2)
       (string<=? (car l1) (car l2))))

(define (compute-sync-list local-base remote-base cont)
  (let* ((rbase (remote-file-name remote-base))
         (server-name (tmfs-car rbase))
         (server (client-find-server server-name))
         (local-l (client-sync-list local-base))
         (t (make-ahash-table)))
    (client-remote-eval server `(remote-sync-list ,rbase)
      (lambda (remote-l)
        (for (local-e local-l)
          (with (dir? name) local-e
            (with d (url->string (url-subtract name local-base))
              (ahash-set! t d (list dir? #t)))))
        (for (remote-e remote-l)
          (with (dir? name id) remote-e
            (let* ((base (tmfs-cdr (remote-file-name remote-base)))
                   (d (url->string (url-subtract name base)))
                   (prev (ahash-ref t d)))
              (cond ((not prev)
                     (ahash-set! t d (list dir? #f id)))
                    ((list-1? prev)
                     (ahash-set! t d (list dir? (cadr prev) id)))))))
        (with l (sort (ahash-table->list t) first-string-leq?)
          (cont l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determine local files which have to be uploaded
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-url-sync-info dir? local-exists? local-name remote-name)
  (with-database (user-database "sync")
    (let* ((server-name (tmfs-car (remote-file-name remote-name)))
           (ids (db-search `(("name" ,(url->system local-name))
                             ("remote-name" ,(url->system remote-name))
                             ("type" "sync")
                             ("server" ,server-name))))
           (date (and local-exists?
                      (number->string (url-last-modified local-name)))))
      (if (nnull? ids)
          (let* ((local-id (car ids))
                 (date* (db-get-field-first local-id "date" #f))
                 (remote-id* (db-get-field-first local-id "remote-id" #f)))
            (list dir? local-name local-id date date* remote-id*))
          (with local-id
              (db-create-entry `(("type" "sync")
                                 ("server" ,server-name)
                                 ("name" ,(url->system local-name))
                                 ("remote-name" ,(url->system remote-name))
                                 ("date" ,date)))
            (list dir? local-name local-id date #f #f))))))

(define (get-sync-status info remote-name remote-id)
  (with (dir? local-name local-id date date* remote-id*) info
    (let* ((local-info (list (url->system local-name) local-id))
           (remote-info (list (remote-file-name remote-name) remote-id))
           (all-info (append (list dir?) local-info remote-info)))
    (cond ((and date (== date date*) (== remote-id remote-id*)) #f)
          ((and date (== date date*) (not remote-id))
           (cons "local-delete" all-info))
          ((and (not date) remote-id (== remote-id remote-id*))
           (cons "remote-delete" all-info))
          ((not date) (cons "download" all-info))
          ((not (and remote-id remote-id*)) (cons "upload" all-info))
          (else (cons "conflict" all-info))))))

(define (url-append* base u)
  (if (== (url->url u) (string->url ".")) base (url-append base u)))

(define (compute-sync-status local-base remote-base cont)
  (compute-sync-list local-base remote-base
    (lambda (l)
      (with r (list)
        (for (x l)
          (with (rname dir? local? . opt) x
            (let* ((local-name (url-append* local-base rname))
                   (remote-name (url-append* remote-base rname))
                   (info (get-url-sync-info dir? local? local-name remote-name))
                   (remote-id (and (nnull? opt) (car opt)))
                   (next (get-sync-status info remote-name remote-id)))
              (when next (set! r (cons next r))))))
        (cont (reverse r))))))

(tm-define (sync-test)
  (compute-sync-status (string->url "~/test/sync-test") (current-buffer)
    (lambda (l)
      (for (x l)
        (display* x "\n")))))
