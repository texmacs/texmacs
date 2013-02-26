
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
;; Remote file manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remote-decode-name rid)
  (with dir "$TEXMACS_HOME_PATH/system/server"
    (when (not (url-exists? dir))
      (system-mkdir dir))
    (string-append dir "/" rid)))

(tm-service (remote-file-create suffix)
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (let* ((rid (resource-create "Nameless remote file" "file" uid))
               (fname (string-append rid "." suffix)))
          (server-return envelope fname)))))

(tm-service (remote-file-load name)
  ;; FIXME: check access rights
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (let* ((rid (tmfs-cdr name))
               (fname (remote-decode-name rid)))
          (if (not (url-exists? fname))
              (server-error envelope "Created new file")
              (let* ((tm (string-load fname))
                     (doc (convert tm "texmacs-document" "texmacs-stree")))
                (server-return envelope doc)))))))

(tm-service (remote-file-save name doc)
  ;; FIXME: check access rights
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (let* ((rid (tmfs-cdr name))
               (fname (remote-decode-name rid))
               (tm (convert doc "texmacs-stree" "texmacs-document")))
          (string-save tm fname)
          (server-return envelope name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-document doc)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic"))
     (body ,doc)))

(define (decode-key-value s)
  (with i (string-index s #\=)
    (if (not i) (list "error" "error")
        (list (substring s 0 i) (substring s (+ i 1) (string-length s))))))

(define (dir-line server rid)
  (let* ((names (resource-get rid "name"))
         (name (if (null? names) "Unknown name" (car names)))
         (fname (string-append "tmfs://remote-file/" server "/" rid ".tm"))
         (hlink `(hlink ,name ,fname)))
    hlink))

(define (dir-page server rids)
  (generic-document `(document ,@(map (cut dir-line server <>) rids))))

(tm-service (remote-dir-load name)
  ;; FIXME: check access rights
  (with uid (server-get-user envelope)
    (if (not uid) (server-error envelope "Error: not logged in")
        (let* ((server (car (tmfs->list name)))
               (pairs (rcons (cdr (tmfs->list name)) "type=file"))
               (query (map decode-key-value pairs))
               (matches (resource-search query)))
          (server-return envelope (dir-page server matches))))))
