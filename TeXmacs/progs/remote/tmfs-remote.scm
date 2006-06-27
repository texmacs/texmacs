
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs-remote.scm
;; DESCRIPTION : remote TeXmacs file systems
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs-remote)
  (:use (remote client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfs-server)
  (or (logged-server) (get-server) (default-server)))

(define (url->name u)
  (if (url? u) (set! u (url->string u)))
  (if (string-starts? u "tmfs://") (set! u (string-drop u 7)))
  (if (string-index u #\/) u (string-append "file/" u)))

(define (url->projects u)
  (if (not (remote-buffer? u)) '()
      (with-server (tmfs-server)
	(remote-request `(tmfs-get-properties ,(url->name u) project)))))

(tm-define (remote-new-file name)
  (:synopsis "Create a file with a given @name on the remote file server.")
  (:argument name "File name")
  (with-server (tmfs-server)
    (and-let* ((style (get-style-tree))
	       (prjs (url->projects (get-name-buffer)))
	       (u (remote-request `(tmfs-new ,name "scm")))
	       (file (url->name u)))
      (new-buffer)
      (set-name-buffer u)
      (set-abbr-buffer (remote-name u))
      (set-style-tree style)
      (remote-request `(tmfs-set-properties ,file project ,@prjs))
      (when (nnull? prjs)
	(remote-request `(tmfs-set-properties ,file owner "^project"))))))

(tm-define (remote-load u)
  (with-server (tmfs-server)
    (remote-request `(tmfs-load ,(url->name u)))))

(tm-define (remote-save u what)
  (with-server (tmfs-server)
    (when (remote-request `(tmfs-save ,(url->name u) ,what))
      (pretend-save-buffer))))

(tm-define (remote-name u)
  (with-server (tmfs-server)
    (remote-request `(tmfs-name ,(url->name u)))))

(tm-define (remote-permission? u prop)
  (with-server (tmfs-server)
    (set! prop (locase-all prop))
    (and-with type (and (string? prop) (string->symbol prop))
      (remote-request `(tmfs-permission? ,(url->name u) ,type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique properties of the current document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-buffer? . opt-u)
  (with u (if (null? opt-u) (get-name-buffer) (car opt-u))
    (and (not (url-none? u))
	 (string-starts? (url->string u) "tmfs://")
	 (tmfs-remote? u))))

(tm-define (remote-get-property-types)
  (:synopsis "Get the list of remote property types for the current buffer.")
  (with-server (tmfs-server)
    (with name (url->name (get-name-buffer))
      (remote-request `(tmfs-get-property-types ,name)))))

(tm-define (remote-get-property prop)
  (:synopsis "Get the remote property @prop for the current buffer.")
  (:argument prop "Property")
  (with-server (tmfs-server)
    (set! prop (locase-all prop))
    (and-let* ((name (url->name (get-name-buffer)))
	       (type (and (string? prop) (string->symbol prop)))
	       (vals (remote-request `(tmfs-get-properties ,name ,type))))
      (string-recompose-comma vals))))

(define (remote-test-property? prop val)
  (== (remote-get-property prop) val))

(tm-define (remote-set-property prop val)
  (:synopsis "Set the remote property @prop for the current buffer to @val.")
  (:argument prop "Property")
  (:argument val "Value")
  (:check-mark "v" remote-test-property?)
  (with-server (tmfs-server)
    (set! prop (locase-all prop))
    (and-let* ((name (url->name (get-name-buffer)))
	       (type (and (string? prop) (string->symbol prop)))
	       (vals (if (== val "") '() (string-tokenize-comma val)))
	       (ok (remote-request `(tmfs-set-properties ,name ,type ,@vals))))
      (if (== type 'name) (set-abbr-buffer val))
      #t)))

(tm-define (interactive-remote-set-property prop)
  (:interactive #t)
  (interactive (lambda (val) (remote-set-property prop val))
    (list (upcase-first prop) "string" (remote-get-property prop))))

(tm-define (interactive-remote-set-property-and-value)
  (:interactive #t)
  (interactive (lambda (prop) (interactive-remote-set-property prop))
    (list "Property" "string")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-new-project name)
  (:synopsis "Create a new projet with a given @name.")
  (:argument name "Project name")
  (with-server (tmfs-server)
    (and-with u (remote-request `(tmfs-new-classifier "project" ,name))
      (load-buffer u))))

(tm-define (remote-get-projects)
  (:synopsis "Get the list of projects for the current user.")
  (with-server (tmfs-server)
    (remote-request `(tmfs-classifiers "project"))))

(tm-define (remote-project-load-by-name name)
  (:synopsis "Go to the file with a given @name in the current project.")
  (:secure #t)
  (with-server (tmfs-server)
    (when (remote-buffer?)
      (let* ((this (url->name (get-name-buffer)))
	     (u (remote-request `(tmfs-project-search-name ,this ,name))))
	(cursor-history-add (cursor-path))
	(if u (load-buffer u) (remote-new-file name))
	(cursor-history-add (cursor-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File transfer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-like? u)
  (in? (url-suffix u) '("tm" "ts" "tmml")))

(define (load-following-suffix u)
  (if (tm-like? u)
      (object->string (tree->stree (texmacs-load-tree u "generic")))
      (string-load u)))

(tm-define (remote-export local-url remote-name)
  (:synopsis "Export @local-url to a remote file with name @remote-name.")
  (with-server (tmfs-server)
    (and-let* ((prjs (url->projects (get-name-buffer)))
	       (suffix (if (tm-like? local-url) "scm" (url-suffix local-url)))
	       (u (remote-request `(tmfs-new ,remote-name ,suffix)))
	       (v (remote-request `(tmfs-revision ,(url->name u) main)))
	       (file (url->name v))
	       (s (load-following-suffix local-url))
	       (saved (remote-request `(tmfs-save ,(url->name v) ,s))))	       
      (remote-request `(tmfs-set-properties ,file project ,@prjs))
      (when (nnull? prjs)
	(remote-request `(tmfs-set-properties ,file owner "^project")))
      (load-buffer u))))

(tm-define (interactive-remote-export u)
  (:interactive #t)
  (interactive (lambda (remote-name) (remote-export u remote-name))
    (list "Remote file name" "string")))

(tm-define (remote-import remote-url local-url)
  (:synopsis "Import the current remote buffer to the local file system.")
  (if (tm-like? local-url)
      (begin
	(load-buffer remote-url)
	(delayed
	  (:pause 10)
	  (save-buffer local-url "generic")))
      (with-server (tmfs-server)
	(and-let* ((name (url->name remote-url))
		   (u (remote-request `(tmfs-revision ,name main)))
		   (s (remote-request `(tmfs-load ,(url->name u)))))
	  (string-save s local-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-home-directory)
  (and-let* ((user (remote-user))
	     (name (string-append "tmfs://dir/owner=" user)))
    (load-buffer name)))

(tm-define (remote-file-information)
  (and-let* ((remote (remote-buffer?))
	     (u (get-name-buffer)))
    (with (class name) (tmfs-decompose-name u)
      (when (== class "file")
	(with info (string-append "tmfs://file-info/" name)
	  (load-buffer info))))))
