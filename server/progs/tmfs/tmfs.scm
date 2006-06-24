
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
(use-modules (srfi srfi-1)
	     (tools base) (tools abbrevs) (tools ahash-table)
	     (tools list) (tools string) (tools file)
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
    (if i (substring s 0 i) s)))

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
;; Generation of directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-property-types file)
  (let* ((sols (property-query `(:t ,file :v)))
	 (r (map (lambda (l) (assoc-ref l :t)) sols)))
    (list-remove-duplicates r)))

(define (files-property-types files)
  (with ts (apply append (map file-property-types files))
    (list-remove-duplicates ts)))

(define (file-property-values file type)
  (let* ((sols (property-query `(,type ,file :v)))
	 (r (map (lambda (l) (assoc-ref l :v)) sols)))
    (list-remove-duplicates r)))

(define (files-property-values files type)
  (with ts (apply append (map (cut file-property-values <> type) files))
    (list-remove-duplicates ts)))

(define (dir-type-make-link type val a)
  (let* ((b (alist-copy a))
	 (c (if (== val "any") (assoc-remove! b type) (assoc-set! b type val)))
	 (d (sort c (lambda (x y) (string<=? (car x) (car y)))))
	 (url (string-append "tmfs://dir?" (alist->string d))))
    `(hlink ,val ,url)))

(define (dir-type->document type a files)
  (let* ((type-s (symbol->string type))
	 (val (assoc-ref a type-s))
	 (val-text (if val val "any"))
	 (alts (if val (list "any") (files-property-values files type)))
	 (links (map (cut dir-type-make-link type-s <> a) alts)))
    `(concat ,(symbol->string type)
	     ,(string-append " = " val-text " (")
	     ,@(list-intersperse links ", ")
	     ")")))

(define (dir-entry->document file)
  (let* ((user (current-user))
	 (suffixes (file-get-properties file 'type))
	 (suffix (if (null? suffixes) "scm" (car suffixes)))
	 (class (if (== suffix "scm") "file" "file-info"))
	 (url (string-append "tmfs://" class "?" file "." suffix))
	 (names (file-get-properties file 'name))
	 (name (if (null? names) "No name" (car names))))
    `(hlink ,name ,url)))

(define (dir->document props)
  (let* ((user (current-user))
	 (a (if (== props "") '() (string->alist props)))
	 (l (map (lambda (x) `(,(string->symbol (car x)) :f ,(cdr x))) a))
	 (sols (map cdar (apply property-query l)))
	 (fs (list-filter sols (cut file-allow? <> user 'read)))
	 (ts1 (map string->symbol (map car a)))
	 (ts2 (list-union (files-property-types fs) ts1))
	 (ts3 (list-difference ts2 '(name date read write)))
	 (ts4 (sort ts3 symbol<=?)))
    `(document
      (tmdoc-title "Directory")
      (concat (vspace* "0.5em") (strong (large "Properties")))
      ,@(map (cut dir-type->document <> a fs) ts4)
      (concat (vspace* "0.5em") (strong (large "Matches")))
      ,@(map dir-entry->document fs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic file manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-new system-name user-name)
  (and-let* ((file (name->file system-name))
	     (suffix (name->suffix system-name))
	     (user (current-user))
	     (ok (null? (file-get-properties file 'owner))))
    (file-set-properties file 'owner user)
    (file-set-properties file 'name user-name)
    (file-set-properties file 'date (number->string (current-time)))
    (file-set-properties file 'type suffix)
    (string-append "tmfs://file?" file "." suffix)))

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
	((== (name->class name) "dir")
	 (tmfs-document (dir->document (name-trim-class name))))
	(else #f)))

(request-handler (tmfs-save name s)
  (when (== (name->class name) "file")
    (let* ((file (name->file name))
	   (suffix (name->suffix name))
	   (user (current-user)))
      (when (file-allow? file user 'write)
	(when (== (file-get-properties file 'type) (list suffix))
	  (file-set file s)
	  (file-set-properties file 'date (number->string (current-time)))
	  #t)))))

(request-handler (tmfs-name name)
  (cond ((== (name->class name) "file")
	 (with names (tmfs-get-properties name 'name)
	   (and names (nnull? names) (car names))))
	((== (name->class name) "file-info")
	 (with name* (string-append "file?" (name-trim-class name))
	   (and-with s (tmfs-name name*)
	     (string-append "Information - " s))))
	((== (name->class name) "dir")
	 (string-append "Directory - " (name-trim-class name)))
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
	((== (name->class name) "dir")
	 (!= (name-trim-class name) ""))
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
