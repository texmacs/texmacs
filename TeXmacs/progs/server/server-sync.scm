
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-sync.scm
;; DESCRIPTION : synchronizing server files with clients
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-sync)
  (:use (server server-db-sync)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dir-contents dir)
  (db-search `(("dir" ,dir)
               (:order "name" #t))))

(define (remote-file-name fname)
  (set! fname (url->string fname))
  (cond ((string-starts? fname "tmfs://remote-file/")
         (substring fname 19 (string-length fname)))
        ((string-starts? fname "tmfs://remote-dir/")
         (substring fname 18 (string-length fname)))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build list of files to be synchronized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (server-sync-list rid uid)
  ;;(display* "  Visiting " rid ", " (resource->file-name rid) "\n")
  (if (not (db-allow? rid uid "readable")) (list)
      (if (== (db-get-field-first rid "type" #f) "dir")
          (with l (dir-contents rid)
            (cons (list #t (resource->file-name rid) rid)
                  (append-map (cut server-sync-list <> uid) l)))
          (list (list #f (resource->file-name rid) rid)))))

(tm-service (remote-sync-list rname)
  ;;(display* "remote-sync-list " rname "\n")
  (with-remote-context rname
    (with uid (server-get-user envelope)
      (if (not uid) (server-error envelope "Error: not logged in")
          (with rid (file-name->resource (tmfs-cdr rname))
            (cond ((not rid)
                   (server-return envelope (list)))
                  ((not (db-allow? rid uid "readable"))
                   (server-error envelope "Error: read access required"))
                  (else
                    (with l (server-sync-list rid uid)
                      (server-return envelope l)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uploading lists of files to the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-upload-one uid line msg)
  ;;(display* "  remote-upload-one " uid ", " line ", " msg "\n")
  (with (cmd dir? local-name local-id remote-name remote-id doc) line
    (let* ((rname (remote-file-name remote-name))
           (rid (file-name->resource (tmfs-cdr rname))))
      (cond ((and dir? (not rid))
             (with r (server-dir-create uid rname)
               (and (== (car r) :created) (cadr r))))
            ((and dir? rid) rid)
            ((not rid)
             (with r (server-file-create uid rname doc msg)
               (and (== (car r) :created) (cadr r))))
            (else
             (with r (server-file-save uid rname doc msg)
               (and (== (car r) :created) (cadr r))))))))

(tm-service (remote-upload l msg)
  ;;(display* "remote-upload " l ", " msg "\n")
  (if (null? l)
      (server-return envelope l)
      (with rname (remote-file-name (fifth (car l)))
        (with-remote-context rname
          (with uid (server-get-user envelope)
            (cond ((not uid) (server-error envelope "Error: not logged in"))
                  (past? (server-error envelope "Error: cannot modify past"))
                  (else
                    (with r (map (cut remote-upload-one uid <> msg) l)
                      (server-return envelope r)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Downloading lists of files from the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-download-one uid line)
  ;;(display* "  remote-download-one " uid ", " line "\n")
  (with (cmd dir? local-name local-id remote-name remote-id) line
    (let* ((rname (remote-file-name remote-name))
           (rid (file-name->resource (tmfs-cdr rname))))
      (if dir? (list remote-id "")
          (with r (server-file-load uid rname)
            (and (== (car r) :loaded)
                 (list remote-id (cadr r))))))))

(tm-service (remote-download l)
  ;;(display* "remote-download " l "\n")
  (if (null? l)
      (server-return envelope l)
      (with rname (remote-file-name (fifth (car l)))
        (with-remote-context rname
          (with uid (server-get-user envelope)
            (if (not uid) (server-error envelope "Error: not logged in")
                (with r (map (cut remote-download-one uid <>) l)
                  (server-return envelope r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing lists of files from the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-remove-one uid line)
  ;;(display* "  remote-remove-one " uid ", " line "\n")
  (with (cmd dir? local-name local-id remote-name remote-id) line
    (let* ((rname (remote-file-name remote-name))
           (rid (file-name->resource (tmfs-cdr rname))))
      (if dir?
          (with r (server-dir-remove uid rname #f)
            (and (== (car r) :removed)
                 (list remote-id :removed)))
          (with r (server-file-remove uid rname)
            (and (== (car r) :removed)
                 (list remote-id :removed)))))))

(tm-service (remote-remove-several l)
  ;;(display* "remote-remove-several " l "\n")
  (if (null? l)
      (server-return envelope l)
      (with rname (remote-file-name (fifth (car l)))
        (with-remote-context rname
          (with uid (server-get-user envelope)
            (if (not uid) (server-error envelope "Error: not logged in")
                (with r (map (cut remote-remove-one uid <>) l)
                  (server-return envelope r))))))))
