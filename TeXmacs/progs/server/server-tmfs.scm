
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
  (:use (server server-resource)))

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
    (resource-set rid "location" (list tail))
    name))

(define (repository-get rid)
  (and rid
       (with l (resource-get rid "location")
         (and (pair? l) (string-append repo "/" (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-file l . where)
  (if (null? l) where
      (let* ((q (if (null? where)
                    (list "type" "dir")
                    (list "dir" (car where))))
             (matches (resource-search (list (list "name" (car l)) q))))
        (append-map (cut search-file (cdr l) <>) matches))))

(define (dir-contents dir)
  (resource-search (list (list "dir" dir))))

(define (file-name->resource name)
  (safe-car (search-file (tmfs->list name))))

(define (resource->file-name rid)
  (let* ((dir (resource-get-first rid "dir" #f))
         (name (resource-get-first rid "name" "?")))
    (if dir (string-append (resource->file-name dir) "/" name) name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (safe-car l) (and (pair? l) (car l)))

(tm-service (remote-file-create rname doc)
  ;;(display* "remote-file-create " rname ", " doc "\n")
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
          ((not (resource-allow? did uid "writable"))
           (server-error envelope "Error: write access required for directory"))
          (else
            (let* ((rid (resource-create (cAr l) "file" uid))
                   (name (repository-add rid "tm"))
                   (fname (repository-get rid))
                   (tm (convert doc "texmacs-stree" "texmacs-document")))
              (resource-set rid "dir" (list did))
              (string-save tm fname)
              (with props (resource-get-all-decoded rid)
                (server-return envelope (list doc props))))))))

(tm-service (remote-file-load rname)
  ;;(display* "remote-file-load " rname "\n")
  (let* ((uid (server-get-user envelope))
         (rid (file-name->resource (tmfs-cdr rname)))
         (fname (repository-get rid)))
    (cond ((not uid) ;; FIXME: anonymous access
           (server-error envelope "Error: not logged in"))
          ((not rid)
           (server-error envelope "Error: file does not exist"))
          ((not (resource-allow? rid uid "readable"))
           (server-error envelope "Error: read access denied"))
          ((not (url-exists? fname))
           (server-error envelope "Error: file not found"))
          (else
            (let* ((props (resource-get-all-decoded rid))
                   (tm (string-load fname))
                   (doc (convert tm "texmacs-document" "texmacs-stree")))
              (server-return envelope (list doc props)))))))

(tm-service (remote-file-save rname doc)
  ;;(display* "remote-file-save " rname ", " doc "\n")
  (let* ((uid (server-get-user envelope))
         (rid (file-name->resource (tmfs-cdr rname)))
         (fname (repository-get rid))
         (tm (convert doc "texmacs-stree" "texmacs-document")))
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          ((not rid)
           (server-error envelope "Error: file does not exist"))
          ((not (resource-allow? rid uid "writable"))
           (server-error envelope "Error: write access denied"))
          (else
            (with props (resource-get-all-decoded rid)
              (string-save tm fname)
              (server-return envelope (list doc props)))))))

(tm-service (remote-file-set-properties rname props)
  ;;(display* "remote-file-set-properties " rname ", " props "\n")
  (let* ((uid (server-get-user envelope))
         (rid (file-name->resource (tmfs-cdr rname))))
    (cond ((not uid)
           (server-error envelope "Error: not logged in"))
          ((not rid)
           (server-error envelope "Error: file does not exist"))
          ((not (resource-allow? rid uid "owner"))
           (server-error envelope "Error: administrative access denied"))
          (else
            (resource-set-all-encoded rid props)
            (with new-props (resource-get-all-decoded rid)
              (server-return envelope new-props))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter-read-access rids uid)
  (cond ((null? rids) rids)
        ((resource-allow? (car rids) uid "readable")
         (cons (car rids) (filter-read-access (cdr rids) uid)))
        (else (filter-read-access (cdr rids) uid))))

(define (rewrite-dir-entry rid)
  (let* ((short-name (resource-get-first rid "name" "?"))
         (full-name (resource->file-name rid))
         (dir? (== (resource-get-first rid "type" #f) "dir"))
         (props (resource-get-all-decoded rid)))
    (list short-name full-name dir? props)))

(tm-service (remote-dir-load rname)
  ;;(display* "remote-dir-load " rname "\n")
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (let* ((server (car (tmfs->list rname)))
               (dirs (search-file (cdr (tmfs->list rname))))
               (matches (append-map dir-contents dirs))
               (filtered (filter-read-access matches uid)))
          (server-return envelope (map rewrite-dir-entry filtered))))))
