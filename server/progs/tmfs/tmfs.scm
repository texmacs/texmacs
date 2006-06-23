
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs.scm
;; DESCRIPTION : Interface to the TeXmacs file system
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tmfs tmfs))
(use-modules (tools base) (tools abbrevs) (tools ahash-table)
	     (tools list) (tools file)
	     (server request) (server atoms)
	     (tmfs file-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-allow? file user action)
  (or (and user (nnull? (property-query `(,action ,file ,user))))
      (nnull? (property-query `(,action ,file "all")))
      (and (!= action 'owner) (file-allow? file user 'owner))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (name->class name)
  (with i (string-index name #\?)
    (if i (substring name 0 i) "file")))

(define (name-trim-class name)
  (with i (string-index name #\?)
    (if i (substring name (+ i 1) (string-length name)) name)))

(define (name->file name)
  (let* ((s (name-trim-class name))
	 (i (string-index s #\.)))
    (string->number (if i (substring s 0 i) s))))

(define (name->suffix name)
  (let* ((s (name-trim-class name))
	 (i (string-index s #\.)))
    (if i (substring s (+ i 1) (string-length s)) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic file manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-new name suffix)
  (and-with user (current-user)
    (with file (file-new)
      (file-set-properties file 'owner user)
      (file-set-properties file 'name name)
      (file-set-properties file 'date (number->string (current-time)))
      (file-set-properties file 'type suffix)
      (string-append "tmfs://file?" (number->string file) "." suffix))))

(request-handler (tmfs-save name s)
  (when (== (name->class name) "file")
    (let* ((file (name->file name))
	   (suffix (name->suffix name))
	   (user (current-user)))
      (when (file-allow? file user 'write)
	(file-set file s)
	(file-set-properties file 'date (number->string (current-time)))
	(file-set-properties file 'type suffix)
	#t))))

(request-handler (tmfs-load name)
  (when (== (name->class name) "file")
    (let* ((file (name->file name))
	   (suffix (name->suffix name))
	   (user (current-user)))
      (when (file-allow? file user 'read)
	(when (== (file-get-properties file 'type) (list suffix))
	  (file-get file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-property-types)
  (and-with user (current-user)
    (let* ((r (property-query `(owner :f ,user) `(:t :f :y)))
	   (t (map (lambda (l) (assoc-ref l :t)) r)))
      (list-remove-duplicates t))))

(request-handler (tmfs-permission? name type)
  (when (== (name->class name) "file")
    (let* ((file (name->file name))
	   (user (current-user)))
      (file-allow? file user type))))

(request-handler (tmfs-get-properties name type)
  (when (== (name->class name) "file")
    (let* ((file (name->file name))
	   (user (current-user)))
      (when (file-allow? file user 'read)
	(file-get-properties file type)))))

(request-handler (tmfs-set-properties name type . vals)
  (when (== (name->class name) "file")
    (let* ((file (name->file name))
	   (user (current-user)))
      (when (file-allow? file user 'owner)
	(apply file-set-properties (cons* file type vals))
	#t))))
