
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

(tm-define (remote-new-file name)
  (:synopsis "Create a file with a given @name on the remote file server.")
  (:argument name "File name")
  (with-server (tmfs-server)
    (and-with created (remote-request `(tmfs-new ,name))
      (new-buffer)
      (set-name-buffer created)
      (set-abbr-buffer name))))

(tm-define (remote-load name)
  (with-server (tmfs-server)
    (remote-request `(tmfs-load ,name))))

(tm-define (remote-save name what)
  (with-server (tmfs-server)
    (when (remote-request `(tmfs-save ,name ,what))
      (pretend-save-buffer))))

(tm-define (remote-name name)
  (with-server (tmfs-server)
    (with names (remote-request `(tmfs-get-properties ,name name))
      (if (or (not names) (null? names))
	  (string-append "tmfs://" name)
	  (car names)))))

(tm-define (remote-permission? name prop)
  (with-server (tmfs-server)
    (and-with type (and (string? prop) (string->symbol prop))
      (remote-request `(tmfs-permission? ,name ,type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique properties of the current document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-buffer?)
  (with u (get-name-buffer)
    (and (not (url-none? u))
	 (string-starts? (url->string u) "tmfs://")
	 (tmfs-remote? u))))

(tm-define (remote-get-property prop)
  (:synopsis "Get the remote property @prop for the current buffer.")
  (:argument prop "Property")
  (with-server (tmfs-server)
    (and-let* ((name (url->string (get-name-buffer)))
	       (type (and (string? prop) (string->symbol prop)))
	       (vals (remote-request `(tmfs-get-properties ,name ,type))))
      (and (nnull? vals) (string-recompose-comma vals)))))

(tm-define (remote-set-property prop val)
  (:synopsis "Set the remote property @prop for the current buffer to @val.")
  (:argument prop "Property")
  (:argument val "Value")
  (with-server (tmfs-server)
    (and-let* ((name (url->string (get-name-buffer)))
	       (type (and (string? prop) (string->symbol prop)))
	       (vals (string-tokenize-comma val)))
      (remote-request `(tmfs-set-properties ,name ,type ,@vals)))))

(tm-define (interactive-remote-set-property prop)
  (:interactive #t)
  (interactive (lambda (val) (remote-set-property prop val))
    (list prop "string" (remote-get-property prop))))

(tm-define (interactive-remote-set-property-and-value)
  (:interactive #t)
  (interactive (lambda (prop) (interactive-remote-set-property prop))
    (list "Property" "string")))
