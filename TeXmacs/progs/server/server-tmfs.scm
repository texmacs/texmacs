
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
(define repo-seed-val (+ (* 4294967296 (abs (texmacs-time)))))
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
;; File hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (safe-car l) (and (pair? l) (car l)))

(define (search-file l . where)
  (if (null? l) where
      (let* ((q (if (null? where)
                    (list "type" "dir")
                    (list "dir" (car where))))
             (matches (db-search (list (list "name" (car l)) q))))
        (append-map (cut search-file (cdr l) <>) matches))))

(define (dir-contents dir)
  (db-search (list (list "dir" dir))))

(define (file-name->resource name)
  (safe-car (search-file (tmfs->list name))))

(define (resource->file-name rid)
  (let* ((dir (db-get-field-first rid "dir" #f))
         (name (db-get-field-first rid "name" "?")))
    (if dir (string-append (resource->file-name dir) "/" name) name)))

(define (inheritance-reserved-attributes)
  (append (db-reserved-attributes)
          (db-meta-attributes)
          (list "name" "version-list" "version-nr")))

(define (inherit-property? x)
  (nin? (car x) (inheritance-reserved-attributes)))

(define (inherit-properties derived-rid base-rid)
  (let* ((props1 (db-get-entry base-rid))
         (props2 (list-filter props1 inherit-property?)))
    (for (prop props2)
      (db-set-field derived-rid (car prop) (cdr prop)))))

(tm-service (remote-identifier rname)
  ;;(display* "remote-identifier " rname ", " props "\n")
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
;; Unpack the context from the file name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfs-car* f) (or (tmfs-car f) f))
(define (tmfs-cdr* f) (or (tmfs-cdr f) ""))

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
;; Remote file manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-create uid rname vid nr doc)
  (let* ((l (tmfs->list rname))
         (did (safe-car (search-file (cDr (cdr l)))))
         (rid (with-time-stamp #t
                (db-create-entry (list (list "type" "file")
                                       (list "name" (cAr l))
                                       (list "owner" uid)
                                       (list "version-list" vid)
                                       (list "version-nr" nr)
                                       (list "version-by" uid)))))
         (name (repository-add rid (url-suffix rname)))
         (fname (repository-get rid)))
    (inherit-properties rid did)
    (db-set-field rid "dir" (list did))
    (string-save doc fname)))

(tm-service (remote-file-create rname doc)
  ;;(display* "remote-file-create " rname ", " doc "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (fid (file-name->resource (tmfs-cdr rname)))
           (l (tmfs->list rname))
           (did (safe-car (search-file (cDr (cdr l))))))
      (cond ((not uid)
             (server-error envelope "Error: not logged in"))
            (fid
             (server-error envelope "Error: file already exists"))
            ((not did)
             (server-error envelope "Error: directory does not exist"))
            ((not (db-allow? did uid "writable"))
             (server-error envelope "Error: directory write access required"))
            (past?
             (server-error envelope "Error: cannot modify past"))
            (else
              (with vid (version-first uid (cAr l))
                (remote-create uid rname vid "1" doc)
                ;;(display* "Versions " rname ": " (version-get-versions vid) "\n")
                (server-return envelope doc)))))))

(tm-service (remote-file-load rname)
  ;;(display* "remote-file-load " rname "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (rid (file-name->resource (tmfs-cdr rname)))
           (fname (repository-get rid)))
      (cond ((not uid) ;; FIXME: anonymous access
             (server-error envelope "Error: not logged in"))
            ((not rid)
             (server-error envelope "Error: file does not exist"))
            ((not (db-allow? rid uid "readable"))
             (server-error envelope "Error: read access denied"))
            ((not (url-exists? fname))
             (server-error envelope "Error: file not found"))
            (else
              (with doc (string-load fname)
                (server-return envelope doc)))))))

(tm-service (remote-file-save rname doc)
  ;;(display* "remote-file-save " rname ", " doc "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (rid (file-name->resource (tmfs-cdr rname)))
           (vid (version-get-list rid))
           (fname (repository-get rid)))
      (cond ((not uid)
             (server-error envelope "Error: not logged in"))
            ((not rid)
             (server-error envelope "Error: file does not exist"))
            ((not (db-allow? rid uid "writable"))
             (server-error envelope "Error: write access denied"))
            ((!= (version-get-number rid) (version-get-current vid))
             (server-error envelope "Error: version number mismatch"))
            ((== (string-load fname) doc) ;; no changes need to be saved
             (server-return envelope doc))
            (past?
             (server-error envelope "Error: cannot modify past"))
            (else
              (with nr (version-next vid)
                (remote-create uid rname vid nr doc)
                (db-remove-entry rid)
                ;;(display* "Versions " rname ": " (version-get-versions vid) "\n")
                (server-return envelope doc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (remote-dir-create rname)
  ;;(display* "remote-dir-create " rname "\n")
  (with-remote-context rname
    (let* ((uid (server-get-user envelope))
           (fid (file-name->resource (tmfs-cdr rname)))
           (l (tmfs->list rname))
           (did (safe-car (search-file (cDr (cdr l))))))
      (cond ((not uid)
             (server-error envelope "Error: not logged in"))
            (fid
             (server-error envelope "Error: directory already exists"))
            ((not did)
             (server-error envelope "Error: directory does not exist"))
            ((not (db-allow? did uid "writable"))
             (server-error envelope "Error: directory write access required"))
            (past?
             (server-error envelope "Error: cannot modify past"))
            (else
              (with rid (db-create-entry (list (list "type" "dir")
                                               (list "name" (cAr l))
                                               (list "owner" uid)))
                (inherit-properties rid did)
                (db-set-field rid "dir" (list did))
                (server-return envelope (list))))))))

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

(tm-service (remote-dir-load rname)
  ;;(display* "remote-dir-load " rname "\n")
  (with-remote-context rname
    (with uid (server-get-user envelope)
      (if (not uid) (server-error envelope "Error: not logged in")
          (let* ((server (car (tmfs->list rname)))
                 (dirs (search-file (cdr (tmfs->list rname))))
                 (rid (safe-car dirs)))
            (cond ((not rid)
                   (server-error envelope "Error: directory does not exist"))
                  ((not (db-allow? rid uid "readable"))
                   (server-error envelope "Error: read access required"))
                  (else
                    (let* ((matches (dir-contents rid))
                           (filtered (filter-read-access matches uid))
                           (rewr (map rewrite-dir-entry filtered)))
                      (server-return envelope rewr)))))))))
