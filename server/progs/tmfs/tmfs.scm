
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
;; Generation of information about files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-type-value x)
  (cons (symbol->string (assoc-ref x :t))
	(assoc-ref x :v)))

(define (file-property->document x)
  (string-append (car x) ": " (cdr x)))

(define (file-properties->document file)
  (with l (property-query `(:t ,file :v))
    (with r (map get-type-value l)
      `(document 
	 (tmdoc-title "File properties")
	 ,@(map file-property->document r)))))

(define (tmfs-document doc)
  (object->string `(document (TeXmacs "1.0.6.3")
			     (style (tuple "tmdoc"))
			     (body ,doc))))

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

(request-handler (tmfs-load name)
  (cond ((== (name->class name) "file")
	 (let* ((file (name->file name))
		(suffix (name->suffix name))
		(user (current-user)))
	   (when (file-allow? file user 'read)
	     (when (== (file-get-properties file 'type) (list suffix))
	       (file-get file)))))
	((== (name->class name) "file-info")
	 (let* ((file (name->file name))
		(user (current-user)))
	   (when (file-allow? file user 'read)
	     (tmfs-document (file-properties->document (name->file name))))))
	(else #f)))

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

(request-handler (tmfs-name name)
  (cond ((== (name->class name) "file")
	 (with names (tmfs-get-properties name 'name)
	   (and names (nnull? names) (car names))))
	((== (name->class name) "file-info")
	 (with name* (string-append "file?" (name-trim-class name))
	   (and-with s (tmfs-name name*)
	     (string-append "Information - " s))))
	(else #f)))

(request-handler (tmfs-permission? name type)
  (cond ((== (name->class name) "file")
	 (let* ((file (name->file name))
		(user (current-user)))
	   (file-allow? file user type)))
	((== (name->class name) "file-info")
	 (with name* (string-append "file?" (name-trim-class name))
	   (and-with s (tmfs-permission? name* 'read)
	     (== type 'read))))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-property-types)
  (and-with user (current-user)
    (let* ((r (property-query `(owner :f ,user) `(:t :f :y)))
	   (t (map (lambda (l) (assoc-ref l :t)) r)))
      (list-remove-duplicates t))))

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
