
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-tmfs.scm
;; DESCRIPTION : Remote file system, server side
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-tmfs)
  (:use (server server-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repository
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define repo (url-concretize "$TEXMACS_HOME_PATH/server"))
(define repo-seed-val (* 65536 (abs (texmacs-time))))
(define repo-seed (seed->random-state repo-seed-val))

(define (repository-add-into dir name)
  (when (not (url-exists? dir))
    (system-mkdir dir))
  (with rdir (string-append dir "/_")
    (if (not (url-exists? rdir))
        (begin
          (system-mkdir rdir)
          (string-append rdir "/" name))
        (with sub (number->string (random 10 repo-seed))
          (repository-add-into (string-append dir "/" sub) name)))))

(define (repository-add rid suffix)
  (let* ((name (if (== suffix "") rid (string-append rid "." suffix)))
         (full (repository-add-into repo name))
         (tail (substring full (+ (string-length repo) 1)
                               (string-length full))))
    (db-set-field rid "location" (list tail))
    name))

(define (repository-get rid)
  (and rid
       (with l (db-get-field rid "location")
         (and (pair? l) (string-append repo "/" (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unpack the context from the file name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (with-remote-context rname . body)
  `(let* ((path (tmfs->list ,rname))
          (host (car path))
          (head (if (pair? (cdr path)) (cadr path) ""))
          (past? (string-starts? head "time="))
          (tail (if past? (cddr path) (cdr path)))
          (next (list->tmfs (cons host tail)))
          (time (if past? (string-drop head 5) db-time)))
     (with-global db-time time
       (with-global ,rname next
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-file l . where)
  (if (null? l) where
      (let* ((q (if (null? where)
                    (list "type" "dir")
                    (list "dir" (car where))))
             (matches (db-search (list (list "name" (car l)) q))))
        (append-map (cut search-file (cdr l) <>) matches))))

(define (dir-contents dir)
  (db-search `(("dir" ,dir)
               (:order "name" #t))))

(tm-define (file-name->resource name)
  (safe-car (search-file (tmfs->list name))))

(tm-define (resource->file-name rid)
  (let* ((dir (db-get-field-first rid "dir" #f))
         (name (db-get-field-first rid "name" "?")))
    (if dir (string-append (resource->file-name dir) "/" name) name)))

(define (inheritance-reserved-attributes)
  (append (db-reserved-attributes)
          (db-meta-attributes)
          (list "name" "version-list" "version-nr")))

(define (inherit-property? x)
  (nin? (car x) (inheritance-reserved-attributes)))

(define (copy-properties base-rid derived-rid pred?)
  (let* ((props1 (db-get-entry base-rid))
         (props2 (list-filter props1 pred?)))
    (for (prop props2)
      (db-set-field derived-rid (car prop) (cdr prop)))))

(tm-service (remote-identifier rname)
  ;;(display* "remote-identifier " rname "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (rid (file-name->resource (tmfs-cdr rname))))
      (cond ((not uid)
             (server-error envelope "Error: not logged in"))
            ((not rid)
             ;;(server-error envelope "Error: file does not exist")
             (server-return envelope #f))
            ((not (db-allow? rid uid "readable"))
             ;;(server-error envelope "Error: read access denied")
             (server-return envelope #f))
            ((!= db-time :now)
             (server-return envelope (list rid db-time)))
            (else (server-return envelope rid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-encoding-table
  (("version-by" * :pseudos) :users))

(define (version-first uid name)
  (with vname (string-append name "-versions")
    (db-create-entry (list (list "type" "version-list")
                           (list "name" vname)
                           (list "owner" uid)
                           (list "version-current" "1")))))

(define (version-next vid)
  (let* ((cur (db-get-field-first vid "version-current" "0"))
         (next (number->string (+ (string->number cur) 1))))
    (with-user #t
      (db-set-field vid "version-current" (list next))
      next)))

(define (version-get-list rid)
  (with-user #t
    (db-get-field-first rid "version-list" #f)))

(define (version-get-number rid)
  (with-user #t
    (db-get-field-first rid "version-nr" "0")))

(define (version-get-info rid)
  (with-user #t
    (with-time :always
      (let* ((date (db-get-field-first rid "date" #f))
             (name (resource->file-name rid))
             (msg  (db-get-field-first rid "version-msg" #f))
             (by   (with-encoding :pseudos 
                     (db-get-field-first rid "version-by" #f))))
        (list rid date name by msg)))))

(define (version->file-name rid)
  (with (rid* date name by msg) (version-get-info rid)
    (if (not date) name (string-append "time=" date "/" name))))

(define (version-get-current vid)
  (with-user #t
    (db-get-field-first vid "version-current" "0")))

(define (version-get-versions vid)
  (with-user #t
    (with-time :always
      (db-search (list (list "version-list" vid)
                       (list :order "version-nr" #t))))))

(define ((readable-by? uid) info)
  (with (rid date name by msg) info
    (with-time (+ (string->number date) 5)
      (db-allow? rid uid "readable"))))

(tm-service (remote-get-versions rname)
  ;;(display* "remote-get-versions " rname "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (rid (file-name->resource (tmfs-cdr rname)))
           (vid (and rid (version-get-list rid)))
           (vl  (and vid (version-get-versions vid))))
      (cond ((not uid) ;; FIXME: anonymous access
             (server-error envelope "Error: not logged in"))
            ((npair? vl)
             (server-error envelope "Error: file does not exist"))
            (else
              (let* ((info (map version-get-info vl))
                     (filt (list-filter info (readable-by? uid))))
                (server-return envelope filt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-create uid rname vid nr doc msg)
  (let* ((l (tmfs->list rname))
         (did (safe-car (search-file (cDr (cdr l)))))
         (opt-msg (if msg `(("version-msg" ,msg)) `()))
         (rid (with-time-stamp #t
                (db-create-entry `(("type" "file")
                                   ("name" ,(cAr l))
                                   ("owner" ,uid)
                                   ("version-list" ,vid)
                                   ("version-nr" ,nr)
                                   ("version-by" ,uid)
                                   ,@opt-msg))))
         (name (repository-add rid (url-suffix rname)))
         (fname (repository-get rid)))
    (copy-properties did rid inherit-property?)
    (db-set-field rid "dir" (list did))
    (string-save doc fname)
    rid))

(tm-define (server-file-create uid rname doc msg)
  (let* ((fid (file-name->resource (tmfs-cdr rname)))
         (l (tmfs->list rname))
         (did (safe-car (search-file (cDr (cdr l))))))
    (cond ((not uid)
           (list :error "Error: not logged in"))
          (fid
           (list :error "Error: file already exists"))
          ((not did)
           (list :error "Error: directory does not exist"))
          ((not (db-allow? did uid "writable"))
           (list :error "Error: directory write access required"))
          (else
            (let* ((vid (version-first uid (cAr l)))
                   (rid (remote-create uid rname vid "1" doc msg)))
              ;;(display* "Versions " rname ": " (version-get-versions vid) "\n")
              (list :created rid))))))

(tm-service (remote-file-create rname doc msg)
  ;;(display* "remote-file-create " rname ", " doc ", " msg "\n")
  (with-remote-context rname
    (if past?
        (server-error envelope "Error: cannot modify past")
        (let* ((uid (server-get-user envelope))
               (r (server-file-create uid rname doc msg)))
          (if (== (car r) :error)
              (server-error envelope (cadr r))
              (server-return envelope doc))))))

(tm-define (server-file-load uid rname)
  (let* ((rid (file-name->resource (tmfs-cdr rname)))
         (fname (repository-get rid)))
    (cond ((not uid) ;; FIXME: anonymous access
           (list :error "Error: not logged in"))
          ((not rid)
           (list :error "Error: file does not exist"))
          ((not (db-allow? rid uid "readable"))
           (list :error "Error: read access denied"))
          ((not (url-exists? fname))
           (list :error "Error: file not found"))
          (else
            (with doc (string-load fname)
              (list :loaded doc))))))

(tm-service (remote-file-load rname)
  ;;(display* "remote-file-load " rname "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (r (server-file-load uid rname)))
      (if (== (car r) :error)
          (server-error envelope (cadr r))
          (server-return envelope (cadr r))))))

(tm-define (server-file-save uid rname doc msg)
  (let* ((fid (file-name->resource (tmfs-cdr rname)))
         (vid (version-get-list fid))
         (fname (repository-get fid)))
      (cond ((not uid)
             (list :error "Error: not logged in"))
            ((not fid)
             (list :error "Error: file does not exist"))
            ((not (db-allow? fid uid "writable"))
             (list :error "Error: write access denied"))
            ((!= (version-get-number fid) (version-get-current vid))
             (list :error "Error: version number mismatch"))
            ((== (string-load fname) doc) ;; no changes need to be saved
             (server-return envelope doc))
            (else
              (let* ((nr (version-next vid))
                     (rid (remote-create uid rname vid nr doc msg)))
                (copy-properties fid rid inherit-property?)
                (db-remove-entry fid)
                ;;(display* "Versions " rname ": " (version-get-versions vid) "\n")
                (list :created rid))))))

(tm-service (remote-file-save rname doc msg)
  ;;(display* "remote-file-save " rname ", " doc ", " msg "\n")
  (with-remote-context rname
    (if past?
        (server-error envelope "Error: cannot modify past")
        (let* ((uid (server-get-user envelope))
               (r (server-file-save uid rname doc msg)))
          (if (== (car r) :error)
              (server-error envelope (cadr r))
              (server-return envelope doc))))))

(tm-define (server-file-remove uid rname)
  ;;(display* "server-file-remove " uid ", " rname "\n")
  (with rid (file-name->resource (tmfs-cdr rname))
    (cond ((not uid)
           (list :error "Error: not logged in"))
          ((not rid)
           (list :error (string-append
                         "Error: file '" rname "' does not exist")))
          ((not (db-allow? rid uid "writable"))
           (list :error (string-append
                         "Error: write access denied for '" rname "'")))
          (else
            (db-remove-entry rid)
            (list :removed rid)))))

(tm-service (remote-file-remove rname)
  ;;(display* "remote-file-remove " rname "\n")
  (with-remote-context rname
    (if past?
        (server-error envelope "Error: cannot modify past")
        (let* ((uid (server-get-user envelope))
               (r (server-file-remove uid rname)))
          (if (== (car r) :error)
              (server-error envelope (cadr r))
              (server-return envelope "removed"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-dir-create uid rname)
  (let* ((fid (file-name->resource (tmfs-cdr rname)))
         (l (tmfs->list rname))
         (did (safe-car (search-file (cDr (cdr l))))))
    (cond ((not uid)
           (list :error "Error: not logged in"))
          (fid
           (list :error "Error: directory already exists"))
          ((not did)
           (list :error "Error: directory does not exist"))
          ((not (db-allow? did uid "writable"))
           (list :error "Error: directory write access required"))
          (else
            (with rid (db-create-entry (list (list "type" "dir")
                                             (list "name" (cAr l))
                                             (list "owner" uid)))
              (copy-properties did rid inherit-property?)
              (db-set-field rid "dir" (list did))
              (list :created rid))))))

(tm-service (remote-dir-create rname)
  ;;(display* "remote-dir-create " rname "\n")
  (with-remote-context rname
    (if past?
        (server-error envelope "Error: cannot modify past")
        (let* ((uid (server-get-user envelope))
               (r (server-dir-create uid rname)))
          (if (== (car r) :error)
              (server-error envelope (cadr r))
              (server-return envelope (list)))))))

(define (filter-read-access rids uid)
  (cond ((null? rids) rids)
        ((db-allow? (car rids) uid "readable")
         (cons (car rids) (filter-read-access (cdr rids) uid)))
        (else (filter-read-access (cdr rids) uid))))

(define (rewrite-dir-entry rid)
  (let* ((short-name (db-get-field-first rid "name" "?"))
         (full-name (resource->file-name rid))
         (dir? (== (db-get-field-first rid "type" #f) "dir"))
         (props (with-encoding :pseudos (db-get-entry rid))))
    (list short-name full-name dir? props)))

(tm-define (server-dir-load uid rname)
  (if (not uid) (list :error "Error: not logged in")
      (let* ((server (car (tmfs->list rname)))
             (dirs (search-file (cdr (tmfs->list rname))))
             (rid (safe-car dirs)))
        (cond ((not rid)
               (list :error "Error: directory does not exist"))
              ((not (db-allow? rid uid "readable"))
               (list :error "Error: read access required"))
              (else
                (let* ((matches (dir-contents rid))
                       (filtered (filter-read-access matches uid))
                       (rewr (map rewrite-dir-entry filtered)))
                  (list :loaded rewr)))))))

(tm-service (remote-dir-load rname)
  ;;(display* "remote-dir-load " rname "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (r (server-dir-load uid rname)))
      (if (== (car r) :error)
          (server-error envelope (cadr r))
          (server-return envelope (cadr r))))))

(define (server-files-remove uid prefix fids)
  ;;(display* "server-files-remove " uid ", " fids "\n")
  (if (null? fids) (list :removed)
      (with r (server-files-remove uid prefix (cdr fids))
        (if (== (car r) :error) r
            (with rname (string-append prefix "/"
                                       (resource->file-name (car fids)))
              (server-file-remove uid rname))))))

(tm-define (server-dir-remove uid rname recurse?)
  ;;(display* "server-dir-remove " uid ", " rname ", " recurse? "\n")
  (with rid (file-name->resource (tmfs-cdr rname))
    (cond ((not uid)
           (list :error "Error: not logged in"))
          ((not rid)
           (list :error (string-append
                         "Error: directory '" rname "' does not exist")))
          ((not (db-allow? rid uid "writable"))
           (list :error (string-append
                         "Error: write access denied for '" rname "'")))
          (else
            (with fids (db-search `(("dir" ,rid)
                                    (:order "name" #t)))
              (cond (recurse?
                     (with r (server-files-remove uid (tmfs-car rname) fids)
                       (if (== (car r) :error) r
                           (begin
                             (db-remove-entry rid)
                             (list :removed rid)))))
                    ((nnull? fids)
                     (list :error
                           (string-append
                            "Error: directory '" rname "' is non empty")))
                    (else
                      (db-remove-entry rid)
                      (list :removed rid))))))))

(tm-service (remote-dir-remove rname)
  ;;(display* "remote-dir-remove " rname "\n")
  (with-remote-context rname
    (if past?
        (server-error envelope "Error: cannot modify past")
        (let* ((uid (server-get-user envelope))
               (r (server-dir-remove uid rname #t)))
          (if (== (car r) :error)
              (server-error envelope (cadr r))
              (server-return envelope "removed"))))))
