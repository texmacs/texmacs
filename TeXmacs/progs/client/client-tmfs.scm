
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
        (client client-db)
        (version version-tmfs)))

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
  (let* ((s (object->tmstring doc))
         (t (tree-import-loaded s u "stm")))
    (buffer-set u t)
    (buffer-pretend-saved u)))

(define (remote-file-set name doc)
  (with fname (string-append "tmfs://remote-file/" name)
    (buffer-set-stm fname doc)))

(define (remote-dir-set name doc)
  (with fname (string-append "tmfs://remote-dir/" name)
    (buffer-set-stm fname doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names and identifiers of remote files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-file-name fname)
  (set! fname (url->string fname))
  (cond ((string-starts? fname "tmfs://remote-file/")
         (substring fname 19 (string-length fname)))
        ((string-starts? fname "tmfs://remote-dir/")
         (substring fname 18 (string-length fname)))
        (else #f)))

(tm-define (remote-file? fname)
  (set! fname (url->string fname))
  (string-starts? fname "tmfs://remote-file/"))

(tm-define (remote-directory? fname)
  (set! fname (url->string fname))
  (string-starts? fname "tmfs://remote-dir/"))

(tm-define (remote-identifier server u cont)
  ;;(display* "remote-identifier " server ", " u "\n")
  (set! u (or (remote-file-name u) (url->string u)))
  (client-remote-eval server `(remote-identifier ,u) cont))

(tm-define-macro (with-remote-identifier r server u . body)
  `(remote-identifier ,server ,u
                      (lambda (msg) (with ,r msg ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation in remote file systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-root-directory? fname)
  (and (remote-directory? fname)
       (or (== (url->string (url-tail fname)) "remote-dir")
           (== (url->string (url-tail (url-head fname))) "remote-dir")
           (and (string-starts? (url->string (url-tail fname)) "time=")
                (remote-root-directory? (url-head fname))))))

(tm-define (remote-parent fname)
  (with name (remote-file-name fname)
    (cond ((not name) (url-head fname))
          ((remote-root-directory? fname) fname)
          (else
            (with h (url->string (url-head name))
              (string->url (string-append "tmfs://remote-dir/" h)))))))

(tm-define (remote-home-directory? fname)
  (and (remote-directory? fname)
       (not (remote-root-directory? fname))
       (remote-root-directory? (remote-parent fname))))

(tm-define (remote-get-time fname)
  (and-with rname (remote-file-name fname)
    (let* ((name (tmfs-cdr rname))
           (head (tmfs-car name)))
      (if (string-starts? head "time=")
          (string->number (string-drop head 5))
          :now))))

(tm-define (remote-strip-time fname)
  (and-with rname (remote-file-name fname)
    (let* ((name (tmfs-cdr rname))
           (head (tmfs-car name)))
      (if (string-starts? head "time=")
          (string-append "tmfs://remote-file/"
                         (tmfs-car rname) "/" (tmfs-cdr name))
          fname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remote-commit-message #f)

(tm-define (remote-home-directory server)
  (and-let* ((sname (client-find-server-name server))
             (spseudo (client-find-server-pseudo server)))
    (string-append "tmfs://remote-dir/" sname "/~" spseudo)))

(define (prepend-dir server name type)
  (with dir (url->string (current-buffer))
    (when (not (string-starts? dir "tmfs://remote-dir/"))
      (set! dir (remote-home-directory server)))
    (and dir (string-append "tmfs://" type "/"
                            (substring dir 18 (string-length dir))
                            "/" name))))

(tm-define (remote-create-file server fname doc)
  ;;(display* "remote-create-file " server ", " fname ", " doc "\n")
  (let* ((sname (client-find-server-name server))
         (name (substring fname 19 (string-length fname)))
         (tm (convert doc "texmacs-stree" "texmacs-document"))
         (msg remote-commit-message))
    (client-remote-eval server `(remote-file-create ,name ,tm ,msg)
      (lambda (msg)
        (load-document fname))
      (lambda (err)
        (set-message err "create remote file")))))

(tm-define (remote-create-file-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (and-with fname (prepend-dir server name "remote-file")
          (remote-create-file server fname (empty-document))))
    (list "Name" "string" '())))

(tmfs-permission-handler (remote-file name type)
  ;; FIXME: asynchroneous retrieval of file permissions
  #t)

;;(tmfs-format-handler (remote-file name)
;;  ;; FIXME: support for other file formats
;;  "texmacs-file")

(define remote-title-table (make-ahash-table))

(define (append-time title t)
  (if (in? t (list #f :now))
      title
      (string-append title " - " (pretty-time t))))

(tmfs-title-handler (remote-file name doc)
  (let* ((sname (tmfs-car name))
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-file/" name))
         (def-title (url->string (url-tail fname)))
         (t (remote-get-time fname)))
    (with old-title (or (ahash-ref remote-title-table fname)
                        (append-time def-title t))
      (when server
        (with-remote-identifier rid server fname
          (when rid
            (with-remote-get-field new-title server rid "title"
              (set! new-title (if (pair? new-title)
                                  (append-time (car new-title) t)
                                  old-title))
              (when (and (buffer-exists? fname) (!= new-title old-title))
                (buffer-set-title fname new-title))
              (ahash-set! remote-title-table fname new-title)))))
      old-title)))

(tmfs-load-handler (remote-file name)
  (let* ((sname (tmfs-car name))
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-file/" name)))
    (if (not server)
        ;; FIXME: better error handling
        (texmacs-error "remote-file" "invalid server")
        (begin
          (client-remote-eval server `(remote-file-load ,name)
            (lambda (tm)
              (with doc (convert tm "texmacs-document" "texmacs-stree")
                ;;(display* "LOAD ") (write doc) (display* "\n")
                (remote-file-set name doc))
              (set-message "retrieved contents" "load remote file"))
            (lambda (err)
              (set-message err "load remote file")))
          (set-message "loading..." "load remote file")
          (empty-document)))))

(tmfs-save-handler (remote-file name doc)
  ;;(display* "SAVE ") (write doc) (display* "\n")
  (let* ((sname (tmfs-car name))
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-file/" name))
         (msg remote-commit-message))
    (if (not server)
        (texmacs-error "remote-file" "invalid server")
        (with tm (convert doc "texmacs-stree" "texmacs-document")
          (client-remote-eval server `(remote-file-save ,name ,tm ,msg)
            (lambda (saved)
              (set-message "file saved" "save remote file"))
            (lambda (err)
              (set-message err "save remote file")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-create-dir server fname)
  ;;(display* "remote-create-dir " server ", " fname "\n")
  (let* ((sname (client-find-server-name server))
         (name (substring fname 18 (string-length fname))))
    (client-remote-eval server `(remote-dir-create ,name)
      (lambda (entries)
        (load-document fname))
      (lambda (err)
        (set-message err "create remote directory")))))

(tm-define (remote-create-dir-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (and-with fname (prepend-dir server name "remote-dir")
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
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-dir/" name)))
    (if (not server)
        (texmacs-error "remote-file" "invalid server")
        (begin
          (client-remote-eval server `(remote-dir-load ,name)
            (lambda (entries)
              (remote-dir-set name (dir-page sname entries))
              (set-message "retrieved contents" "remote directory"))
            (lambda (err)
              (set-message err "remote directory")))
          (set-message "loading..." "remote directory")
          (empty-document)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other operations on files and directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ancestor? d f)
  (or (== d f)
      (and (!= (remote-parent f) f)
           (ancestor? d (remote-parent f)))))

(tm-define (remote-rename src dest)
  (let* ((src* (remote-file-name src))
         (dest* (remote-file-name dest))
         (src-sv (car (tmfs->list src*)))
         (dest-sv (car (tmfs->list dest*)))
         (server (client-find-server src-sv))
         (action (if (remote-file? src) "rename file" "rename directory"))
         (dir (remote-parent dest))
         (name (url->string (url-tail dest))))
    (with-remote-identifier rid server src
      (with-remote-identifier did server dir
        (with-remote-identifier oid server dest
          (cond ((== dest src)
                 (set-message "same names" action))
                ((!= dest-sv src-sv)
                 (set-message "cannot rename across different servers" action))
                ((not server)
                 (set-message "not connected to server" action))
                ((ancestor? src dest)
                 (set-message "invalid renaming" action))
                ((not rid)
                 "source file not found" action)
                ((not rid)
                 "destination directory not found" action)
                (oid
                 "destination already exists" action)
                (else
                  (remote-set-field server rid "dir" (list did))
                  (remote-set-field server rid "name" (list name))
                  (buffer-rename src dest)
                  (set-message (string-append "renamed as " (url->string dest))
                               action))))))))

(tm-define (remote-remove what)
  (let* ((dir? (remote-directory? what))
         (name (remote-file-name what))
         (server-name (car (tmfs->list name)))
         (server (client-find-server server-name))
         (action (if dir? "remove directory" "remove file"))
         (done (if dir? "directory removed" "file removed"))
         (cmd `(,(if dir? 'remote-dir-remove 'remote-file-remove) ,name)))
    (if (remote-home-directory? what)
        (set-message "not allowed to remove home directory" action)
        (client-remote-eval server cmd
          (lambda (removed)
            (when (buffer-exists? what)
              (buffer-close what))
            (set-message done action))
          (lambda (err)
            (set-message err action))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (versioned? name)
  (:require (remote-file? name))
  #t)

(tm-define (version-status name)
  (:require (remote-file? name))
  (if (buffer-modified? name) "modified" "unmodified"))

(define (build-version-page name prefix l)
  (with u (tmfs-string->url name)
    ($generic
      ($tmfs-title "History of "
                   ($link (url->unix u)
                     ($verbatim (url->system (url-tail u)))))
      ($description-long
        ($for (i (.. 0 (length l)))
          ($with info (list-ref l i)
            ($with (rid date vname by msg) info
              ($let* ((dest (string-append prefix "time=" date "/" vname))
                      (ln ($link dest (number->string (+ i 1))))
                      (dt (pretty-time (string->number date)))
                      (vs ($inline "Version " ln " by " by " on " dt)))
                ($when (or (not msg) (== msg ""))
                  ($begin `(compact-strong-dot-item* ,vs) $lf))
                ($when (and msg (!= msg ""))
                  ($describe-item vs msg))))))))))

(define (compute-remote-versions rname)
  (let* ((name (remote-file-name rname))
         (sname (tmfs-car name))
         (server (client-find-server sname))
         (fname (string-append "tmfs://remote-file/" name))
         (prefix (string-append "tmfs://remote-file/" sname "/")))
    (and server
         (client-remote-eval server `(remote-get-versions ,name)
           (lambda (l)
             (let* ((s (url->tmfs-string rname))
                    (h (string-append "tmfs://history/" s))
                    (doc (build-version-page s prefix l)))
               (buffer-set h doc)
               (buffer-pretend-saved h)))
           (lambda (err)
             (set-message err "get list with remote versions"))))))

(tm-define (version-history u)
  (:require (remote-file? u))
  (compute-remote-versions u)
  (list))

(tm-define (update-buffer name)
  (:require (remote-file? name))
  (revert-buffer name))

(tm-define (commit-buffer-message name msg)
  (:require (remote-file? name))
  (with-global remote-commit-message msg
    (save-buffer name)))

(tm-define (version-interactive-commit name)
  (:require (remote-file? name))
  (commit-buffer name))

(tm-define (version-revision? name)
  (:require (remote-file? name))
  (nin? (remote-get-time name) (list #f :now)))

(tm-define (version-head name)
  (:require (remote-file? name))
  (string->url (remote-strip-time name)))
