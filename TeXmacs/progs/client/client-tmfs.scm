
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-tmfs.scm
;; DESCRIPTION : remote file systems, client side
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-tmfs)
  (:use (client client-base)
        (client client-resource)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-document doc)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic"))
     (body ,doc)))

(define (empty-document)
  (generic-document '(document "")))

(define (buffer-set-stm u doc)
  (let* ((s (unescape-guile (object->string doc)))
         (t (tree-import-loaded s u "stm")))
    (buffer-set u t)))

(define (remote-file-set name doc)
  (with fname (string-append "tmfs://remote-file/" name)
    (buffer-set-stm fname doc)))

(define (remote-dir-set name doc)
  (with fname (string-append "tmfs://remote-dir/" name)
    (buffer-set-stm fname doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-home-directory server)
  (let* ((sname (client-find-server-name server))
         (sid (client-find-server-user-id server)))
    (string-append "tmfs://remote-dir/" sname "/~" sid)))

(define (prepend-dir server name type)
  (with dir (url->string (current-buffer))
    (when (not (string-starts? dir "tmfs://remote-dir/"))
      (set! dir (remote-home-directory server)))
    (string-append "tmfs://" type "/"
                   (substring dir 18 (string-length dir))
                   "/" name)))

(tm-define (remote-create-file server fname doc)
  ;;(display* "remote-create-file " server ", " fname ", " doc "\n")
  (let* ((sname (client-find-server-name server))
         (name (substring fname 19 (string-length fname)))
         (tm (convert doc "texmacs-stree" "texmacs-document")))
    (client-remote-eval server `(remote-file-create ,name ,tm)
      (lambda (msg)
        (load-buffer fname))
      (lambda (err)
        (set-message err "create remote file")))))

(tm-define (remote-create-file-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (with fname (prepend-dir server name "remote-file")
          (remote-create-file server fname (empty-document))))
    (list "Name" "string" '())))

(tmfs-permission-handler (remote-file name type)
  #t)

;; (tmfs-format-handler (remote-file name)
;;   (let* ((sname (tmfs-car name))
;;          (rname (tmfs-cdr name))
;;          (server (client-find-server sname)))
;;     "texmacs"))

(tmfs-title-handler (remote-file name doc)
  (let* ((sname (tmfs-car name))
         (fname (string-append "tmfs://remote-file/" name)))
    (resource-cache-get-first fname "name" "Nameless remote file")))

(tmfs-load-handler (remote-file name)
  (let* ((sname (tmfs-car name))
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-file/" name)))
    (if (not server)
        ;; FIXME: better error handling
        (texmacs-error "remote-file" "invalid server")
        (begin
          (client-remote-eval server `(remote-file-load ,name)
            (lambda (msg)
              (with (tm props) msg
                (with doc (convert tm "texmacs-document" "texmacs-stree")
                  ;;(display* "LOAD ") (write doc) (display* "\n")
                  (resource-cache-set-all fname props)
                  (remote-file-set name doc))))
            (lambda (err)
              (set-message err "load remote file")))
          (set-message "loading..." "load remote file")
          (empty-document)))))

(tmfs-save-handler (remote-file name doc)
  ;;(display* "SAVE ") (write doc) (display* "\n")
  (let* ((sname (tmfs-car name))
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-file/" name)))
    (if (not server)
        (texmacs-error "remote-file" "invalid server")
        (with tm (convert doc "texmacs-stree" "texmacs-document")
          (client-remote-eval server `(remote-file-save ,name ,tm)
            (lambda (msg)
              (with (saved props) msg
                (resource-cache-set-all fname props)
                (set-message "file saved" "save remote file")))
            (lambda (err)
              (set-message err "save remote file")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-create-dir server fname)
  (display* "remote-create-dir " server ", " fname "\n")
  (let* ((sname (client-find-server-name server))
         (name (substring fname 18 (string-length fname))))
    (client-remote-eval server `(remote-dir-create ,name)
      (lambda (msg)
        (load-buffer fname))
      (lambda (err)
        (set-message err "create remote directory")))))

(tm-define (remote-create-dir-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (with fname (prepend-dir server name "remote-dir")
          (remote-create-dir server fname)))
    (list "Name" "string" '())))

(define (dir-line sname entry)
  (with (short-name full-name dir? props) entry
    (let* ((type (if dir? "remote-dir" "remote-file"))
           (name (string-append "tmfs://" type "/" sname "/" full-name))
           (hlink `(hlink ,short-name ,name)))
      hlink)))

(define (dir-page sname entries)
  (generic-document `(document (subsection* "File list")
                               ,@(map (cut dir-line sname <>) entries))))

(tmfs-load-handler (remote-dir name)
  ;;(display* "Loading remote dir " name "\n")
  (let* ((sname (car (tmfs->list name)))
         (server (client-find-server sname)))
    (if (not server)
        (texmacs-error "remote-file" "invalid server")
        (begin
          (client-remote-eval server `(remote-dir-load ,name)
            (lambda (entries)
              (remote-dir-set name (dir-page sname entries)))
            (lambda (err)
              (set-message err "remote directory")))
          (set-message "loading..." "remote directory")
          (empty-document)))))
