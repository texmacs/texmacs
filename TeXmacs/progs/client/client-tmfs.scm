
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

(define (empty-document)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic"))
     (body (document ""))))

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

(define (prepend-dir server name)
  (with dir (url->string (current-buffer))
    (when (not (string-starts? dir "tmfs://remote-dir/"))
      (set! dir (remote-home-directory)))
    (string-append "tmfs://remote-file/"
                   (substring dir 18 (string-length dir))
                   "/" name)))

(tm-define (remote-create server fname doc)
  ;;(display* "remote-create " server ", " fname ", " doc "\n")
  (let* ((sname (client-find-server-name server))
         (name (substring fname 19 (string-length fname))))
    (client-remote-eval server `(remote-file-create ,name ,doc)
      (lambda (msg)
        (load-buffer fname))
      (lambda (err)
        (set-message err "create remote file")))))

(tm-define (remote-create-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (with fname (prepend-dir server name)
          (remote-create server fname (empty-document))))
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
              (with (doc props) msg
                ;;(display* "LOAD ") (write doc) (display* "\n")
                (resource-cache-set-all fname props)
                (if doc
                    (remote-file-set name doc)
                    (set-message "created new file" "load remote file"))))
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
        (client-remote-eval server `(remote-file-save ,name ,doc)
          (lambda (msg)
            (with (new-doc props) msg
              (resource-cache-set-all fname props)
              (set-message "file saved" "save remote file")))
          (lambda (err)
            (set-message err "save remote file"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (remote-dir name)
  ;;(display* "Loading remote dir " name "\n")
  (let* ((sname (car (tmfs->list name)))
         (server (client-find-server sname)))
    (if (not server)
        (texmacs-error "remote-file" "invalid server")
        (begin
          (client-remote-eval server `(remote-dir-load ,name)
            (lambda (doc)
              (remote-dir-set name doc))
            (lambda (err)
              (set-message err "remote directory")))
          (set-message "loading..." "remote directory")
          (empty-document)))))
