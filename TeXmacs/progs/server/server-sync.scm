
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
  (:use (server server-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build list of files to be synchronized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dir-contents dir)
  (db-search `(("dir" ,dir)
               (:order "name" #t))))

(define (server-sync-list rid uid)
  ;;(display* "  Visiting " rid ", " (resource->file-name rid) "\n")
  (if (not (db-allow? rid uid "readable")) (list)
      (if (== (db-get-field-first rid "type" #f) "dir")
          (with l (dir-contents rid)
            (append-map (cut server-sync-list <> uid) l))
          (list (list (resource->file-name rid) rid)))))

(tm-service (remote-sync-list rname)
  ;;(display* "remote-sync-list " rname "\n")
  (with-remote-context rname
    (with uid (server-get-user envelope)
      (if (not uid) (server-error envelope "Error: not logged in")
          (with rid (file-name->resource (tmfs-cdr rname))
            (cond ((not rid)
                   (server-return envelope #f))
                  ((not (db-allow? rid uid "readable"))
                   (server-error envelope "Error: read access required"))
                  (else
                    (with l (server-sync-list rid uid)
                      (server-return envelope l)))))))))
