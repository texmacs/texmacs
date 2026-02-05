
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
;; Directory entries cache (to avoid refetching on sort change)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dir-entries-cache (make-ahash-table))

(tm-define (cache-dir-entries url sname server entries)
  (ahash-set! dir-entries-cache url (list sname server entries)))

(tm-define (get-cached-dir-entries url)
  (ahash-ref dir-entries-cache url))

(tm-define (clear-cached-dir-entries url)
  (ahash-remove! dir-entries-cache url))

(define (rebuild-from-cache url)
  (and-with cached (get-cached-dir-entries url)
    (let ((sname   (car cached))
          (server  (cadr cached))
          (entries (caddr cached)))
      (cond
        ((string-starts? url "tmfs://remote-dir/")
         (with name (substring url 18 (string-length url))
           (remote-dir-set name (dir-page sname server entries))
           #t))
        ((string-starts? url "tmfs://shared/")
         (buffer-set-stm url (shared-documents entries))
         #t)
        ((string-starts? url "tmfs://chat-rooms/")
         (buffer-set-stm url (chat-rooms-document sname server entries))
         #t)
        ((string-starts? url "tmfs://live-list/")
         (buffer-set-stm url (live-documents sname server entries))
         #t)
        (else #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-sort-change pref val)
  (when (buffer-exists? (current-buffer))
    (with u (url->string (current-buffer))
      (when (or (string-starts? u "tmfs://remote-dir/")
                (string-starts? u "tmfs://shared/")
                (string-starts? u "tmfs://chat-rooms/")
                (string-starts? u "tmfs://live-list/"))
        (rebuild-from-cache u)))))

(define-preferences
  ("remote-file-browser:sort-field" "type" notify-sort-change)
  ("remote-file-browser:sort-direction" "asc" notify-sort-change))

(tm-define (entry-type-priority type-str)
  (cond ((== type-str "dir") 0)
        ((== type-str "file") 1)
        ((== type-str "chat") 2)
        ((== type-str "live") 3)
        (else 4)))

;; Generic comparator that respects sort direction
(define (make-comparator cmp ascending?)
  (if ascending? cmp (lambda (a b) (cmp b a))))

;; Sort by name (case-insensitive)
(define (compare-by-name name-a name-b)
  (string<? (string-downcase name-a) (string-downcase name-b)))

;; Sort by type, then by name within same type
(define (compare-by-type type-a name-a type-b name-b)
  (let ((prio-a (entry-type-priority type-a))
        (prio-b (entry-type-priority type-b)))
    (if (== prio-a prio-b)
        (compare-by-name name-a name-b)
        (< prio-a prio-b))))

;; Sort by date (timestamps as strings or numbers)
(define (compare-by-date date-a date-b)
  (let ((num-a (cond ((number? date-a) date-a)
                     ((string? date-a) (or (string->number date-a) 0))
                     (else 0)))
        (num-b (cond ((number? date-b) date-b)
                     ((string? date-b) (or (string->number date-b) 0))
                     (else 0))))
    (< num-a num-b)))

(tm-define (get-sort-field)
  (get-preference "remote-file-browser:sort-field"))

(tm-define (get-sort-direction)
  (get-preference "remote-file-browser:sort-direction"))

(tm-define (sort-ascending?)
  (== (get-sort-direction) "asc"))

(tm-define (toggle-sort-direction)
  (set-preference "remote-file-browser:sort-direction"
                  (if (sort-ascending?) "desc" "asc")))

(tm-define (set-sort-field field)
  (set-preference "remote-file-browser:sort-field" field))

;; Generic sort function with accessor functions
;; get-name: extracts name from entry
;; get-type: extracts type from entry (or #f if not applicable)
;; get-date: extracts date from entry
(tm-define (sort-entries entries get-name get-type get-date)
  (let* ((field (get-sort-field))
         (asc? (sort-ascending?))
         (cmp (cond
               ((== field "name")
                (lambda (a b)
                  (compare-by-name (get-name a) (get-name b))))
               ((and (== field "type") get-type)
                (lambda (a b)
                  (compare-by-type (get-type a) (get-name a)
                                   (get-type b) (get-name b))))
               ((== field "date")
                (lambda (a b)
                  (compare-by-date (get-date a) (get-date b))))
               (else
                (lambda (a b)
                  (compare-by-name (get-name a) (get-name b))))))
         (final-cmp (make-comparator cmp asc?)))
    (list-sort entries final-cmp)))

;; Sort directory entries
;; Entry format: (short-name full-name dir? props)
(define (sort-directory-entries entries)
  (sort-entries entries
    (lambda (e) (car e))
    (lambda (e) (if (caddr e) "dir" "file"))
    (lambda (e)
      (and-let* ((props (cadddr e))
                 (date-raw (assoc-ref props "date")))
        (if (pair? date-raw) (car date-raw) date-raw)))))

;; Sort entries by name/date (for chat rooms, live docs)
;; Entry format: (name date) or just name
(tm-define (sort-name-entries entries)
  (sort-entries entries
    (lambda (e) (if (pair? e) (car e) e))
    #f
    (lambda (e) (if (pair? e) (cadr e) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-file-browser-document doc)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic" "remote-file-browser"))
     (body ,doc)))

(define (empty-document)
  (remote-file-browser-document '(document "")))

(tm-define (buffer-set-stm u doc)
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
    (list "Name" "string")))

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
	(std-client-error (string-append "remote file " name
					 " is not accessible"))
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
    (list "Name" "string")))

(tm-define (tmfs-type u)
  (with head (tmfs-car (url->string (url-unroot u)))
    (if (string-starts? head "remote-") (string-drop head 7) head)))

(define (directory-entry sname server entry)
  (with (short-name full-name dir? props) entry
    (let* ((type (if dir? "dir" "file"))
           (icon-name (string-append "tm_cloud_" type ".svg"))
           (link (prepend-dir server short-name (string-append "remote-" type)))
           (share-action (build-table-share-action server link))
           (date-raw (assoc-ref props "date"))
           (date (if (and date-raw (pair? date-raw))
                     (pretty-time (string->number (car date-raw)))
                     "")))
      `(dir-entry ,icon-name ,short-name ,link ,date ,share-action))))

(tm-define (build-table-share-action server u)
  (with action-cmd
    (string-append "(open-permissions-editor "
                   (number->string server) " \""
                   u "\")")
    `(action (dir-entry-icon "tm_cloud_share.svg" "Share...") ,action-cmd)))


(tm-define (sort-header-label field label)
  (let* ((current-field (get-sort-field))
         (asc? (sort-ascending?))
         (is-current? (== field current-field))
         (indicator
           (if is-current?
             (if asc? '<blacktriangleup> '<blacktriangledown>)
             '<vartriangleright>)))
    (cond
      ;; Type field: show indicator in 12pt-wide box
      ((== field "type")
       `(resize ,indicator "" "" "12pt" ""))
      ;; Other fields: show label + indicator
      (else
       `(concat ,label " " ,indicator)))))

;; Build clickable sort action
(tm-define (sort-header-action field)
  (let ((current-field (get-sort-field)))
    (if (== field current-field)
        "(toggle-sort-direction)"
        (string-append "(set-sort-field \"" field "\")"))))

(tm-define (build-dir-table title date-label content share?)
  (let* ((type-label (sort-header-label "type" "Type"))
         (type-action (sort-header-action "type"))
         (name-label (sort-header-label "name" "Name"))
         (name-action (sort-header-action "name"))
         (date-hdr-label (sort-header-label "date" date-label))
         (date-action (sort-header-action "date"))
         (share-hdr-label (if share? 'phantom-icon "")))
    `(compact (document (tmfs-title ,title)
                        (dir-header ,type-label ,type-action
                                    ,name-label ,name-action
                                    ,date-hdr-label ,date-action ,share-hdr-label)
                        ,@content))))

(define (directory-table sname server entries)
  (let ((sorted (sort-directory-entries entries)))
    (build-dir-table "My Files" "Date" (map (cut directory-entry sname server <>) sorted) #t)))

(define (dir-page sname server entries)
  (remote-file-browser-document
    `(document
       (dir-list ,(directory-table sname server entries)))))

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
              (cache-dir-entries fname sname server entries)
              (remote-dir-set name (dir-page sname server entries))
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
                  ;; Clear server message cache so renamed resources are resolved correctly
                  (client-remote-eval server '(remote-chat-room-messages-reset)
                    (lambda (ok) (noop))
                    (lambda (err) (noop)))
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
