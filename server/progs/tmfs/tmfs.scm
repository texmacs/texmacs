
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

(define (or-map pred? l)
  (and (nnull? l)
       (or (pred? (car l))
	   (or-map pred? (cdr l)))))

(define (file-allow-via? via file user type)
  ;;(display* "file-allow-via? " via ", " file ", " user ", " type "\n")
  (or (and user (in? via (list user "logged")))
      (== via "all")
      (and (string-starts? via "+")
	   (file-allow? via user type))
      (and (string-starts? via "^")
	   (let* ((new-type (string->symbol (string-drop via 1)))
		  (vals (file-get-properties file new-type))
		  (fs (list-filter vals (cut string-starts? <> "+"))))
	     (or-map (cut file-allow? <> user new-type) fs)))))

(define (file-allow? file user type)
  ;;(display* "file-allow? " file ", " user ", " type "\n")
  (with vals (file-get-properties file type)
    (if (nnull? vals)
	(or-map (cut file-allow-via? <> file user type) vals)
	(or (with prjs (file-get-properties file 'project)
	      (or-map (cut file-allow? <> user type) prjs))
	    (and (!= type 'owner) (file-allow? file user 'owner))))))	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (name->class name)
  (with i (string-index name #\/)
    (if i (substring name 0 i) "file")))

(define (name-trim-class name)
  (with i (string-index name #\/)
    (if i (substring name (+ i 1) (string-length name)) name)))

(define (name->file name)
  (let* ((s (name-trim-class name))
	 (i (string-index s #\.)))
    (if i (substring s 0 i) s)))

(define (name->suffix name)
  (let* ((s (name-trim-class name))
	 (i (string-index s #\.)))
    (if i (substring s (+ i 1) (string-length s)) "")))

(define (file->name file)
  (with names (file-get-properties file 'name)
    (if (null? names) "No name" (car names))))

(define (file->suffix file)
  (with suffixes (file-get-properties file 'type)
    (if (null? suffixes) "scm" (car suffixes))))

(define (file->url file . mode)
  (let* ((suffix (file->suffix file))
	 (faithful? (or (== suffix "scm") (== mode '(main))))
	 (info? (or (== mode '(info)) (not faithful?)))
	 (class (if info? "file-info" "file")))
    (if (!= suffix "") (set! suffix (string-append "." suffix)))
    (string-append "tmfs://" class "/" file suffix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation of information about files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (decode-typed-value val type)
  (cond ((== val "any") val)
	((== type "date")
	 (strftime "%c" (localtime (string->number val))))
	((string-starts? val "+")
	 (with new-vals (file-get-properties val 'classify-value)
	   (if (null? new-vals) val
	       (decode-typed-value (car new-vals) type))))
	(else val)))

(define image-suffixes
  '("ps" "eps" "bmp" "gif" "ico" "tga" "pcx" "wbmp" "wmf" "jpg"
    "jpeg" "png" "tif" "jbig" "ras" "pnm" "jp2" "jpc" "pgx"
    "cut" "iff" "lbm" "jng" "koa" "mng" "pbm" "pcd" "pcx"
    "pgm" "ppm" "psd" "tga" "tiff" "xbm" "xpm"))

(define (get-type-value x)
  (cons (symbol->string (assoc-ref x :t))
	(assoc-ref x :v)))

(define (make-section name)
  `(concat (vspace* "0.5em") (strong (large ,name))))

(define (file-property->document x)
  (string-append (string-upcase-first (car x)) " = "
		 (decode-typed-value (cdr x) (car x))))

(define (get-revision x)
  (cons (number->string (assoc-ref x :n))
	(assoc-ref x :r)))

(define (revision->document x)
  (let* ((by-l (file-get-properties (cdr x) 'by))
	 (by (if (null? by-l) "Unknown" (car by-l)))
	 (date-l (file-get-properties (cdr x) 'date))
	 (date (if (null? by-l) "Unknown"
		   (strftime "%c" (localtime (string->number (car date-l)))))))
    `(concat
       (hlink ,(string-append "Revision " (car x))
	      ,(file->url (cdr x)))
       ,(string-append " by " by " at " date))))

(define (file-properties->document file)
  (let* ((l1 (property-query `(:t ,file :v)))
	 (l2 (map get-type-value l1))
	 (l3 (cons (cons "url" (file->url file 'main)) l2))
	 (l4 (sort l3 (lambda (x y) (string<=? (car x) (car y)))))
	 (r1 (property-query `(revision ,file :r :n)))
	 (r2 (map get-revision r1))
	 (r3 (sort r2 (lambda (x y) (string<=? (car x) (car y))))))
      `(document 
	 (tmdoc-title "File information")
	 ,(make-section "Properties")
	 ,@(map file-property->document l4)
	 ,@(if (nin? (file->suffix file) image-suffixes) '()
	       `(,(make-section "Contents")
		 (postscript ,(file->url file 'main) "" "" "" "" "" "")))
	 ,@(if (<= (length r3) 1) '()
	       `(,(make-section "History")
		 ,@(reverse (map revision->document r3)))))))

(define (tmfs-document doc)
  (object->string `(document (TeXmacs "1.0.6.3")
			     (style (tuple "tmdoc"))
			     (body ,doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation of directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dir-name-rewrite-entry x)
  (cons (string-upcase-first (car x))
	(decode-typed-value (cdr x) (car x))))

(define (dir-name dir1)
  (let* ((l1 (string->alist dir1))
	 (l2 (map dir-name-rewrite-entry l1))
	 (dir2 (alist->string l2))
	 (nice (string-recompose (string-tokenize dir2 #\/) " - ")))
    (string-append "Directory - " nice)))

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
	 (url (string-append "tmfs://dir/" (alist->string d))))
    `(hlink ,(decode-typed-value val type) ,url)))

(define (dir-type->document type a files)
  (let* ((type-s (symbol->string type))
	 (val (assoc-ref a type-s))
	 (val-text (if val (decode-typed-value val type-s) "any"))
	 (alts (if val (list "any") (files-property-values files type)))
	 (links (map (cut dir-type-make-link type-s <> a) alts)))
    `(concat ,(string-upcase-first (symbol->string type))
	     ,(string-append " = " val-text " (")
	     ,@(list-intersperse links ", ")
	     ")")))

(define (dir-entry->document file)
  `(hlink ,(file->name file) ,(file->url file)))

(define (std-property-types)
  '(name date read write classify-type classify-value))

(define (dir->document props)
  (let* ((user (current-user))
	 (a (if (== props "") '() (string->alist props)))
	 (l (map (lambda (x) `(,(string->symbol (car x)) :f ,(cdr x))) a))
	 (sols (map cdar (apply property-query l)))
	 (r (list-filter sols (cut file-allow? <> user 'read)))
	 (fs (sort r (lambda (x y) (string<=? (file->name x) (file->name y)))))
	 (ts1 (map string->symbol (map car a)))
	 (ts2 (list-union (files-property-types fs) ts1))
	 (ts3 (list-difference ts2 (std-property-types)))
	 (ts4 (sort ts3 symbol<=?)))
    `(document
      (tmdoc-title "Directory")
      ,(make-section "Properties")
      ,@(map (cut dir-type->document <> a fs) ts4)
      ,(make-section "Matches")
      ,@(map dir-entry->document fs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic file manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-new name suffix)
  (and-let* ((file (create-unique-id))
	     (user (current-user))
	     (ok (null? (file-get-properties file 'owner))))
    (file-set-properties file 'owner user)
    (file-set-properties file 'name name)
    (file-set-properties file 'type suffix)
    (file-set-properties file 'date (number->string (current-time)))
    (file->url file)))

(request-handler (tmfs-revision name revision)
  (when (in? (name->class name) '("file" "file-info"))
    (let* ((file (name->file name))
	   (user (current-user)))
      (when (file-allow? file user 'read)
	(cond ((== revision 'default) (file->url file))
	      ((== revision 'main) (file->url file 'main))
	      ((== revision 'info) (file->url file 'info))
	      (else (let* ((s (property-query `(revision ,file :r ,revision)))
			   (r (map (lambda (l) (assoc-ref s :r)) s)))
		      (and (nnull? r) (file->url (car r))))))))))

(request-handler (tmfs-load name)
  (cond ((== (name->class name) "file")
	 (let* ((file (name->file name))
		(suffix (name->suffix name))
		(user (current-user)))
	   (when (file-allow? file user 'read)
	     (when (in? (file-get-properties file 'type) `((,suffix) ()))
	       (with s (file-get-last file)
		 (or s (and (== suffix "scm")
			    (tmfs-document '(document "")))))))))
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
	  (file-set-last file s)
	  (file-set-properties file 'date (number->string (current-time)))
	  #t)))))

(request-handler (tmfs-name name)
  (cond ((== (name->class name) "file")
	 (and-with names (tmfs-get-properties name 'name)
	   (and (nnull? names) (car names))))
	((== (name->class name) "file-info")
	 (with name* (string-append "file/" (name-trim-class name))
	   (and-with s (tmfs-name name*)
	     (string-append "Information - " s))))
	((== (name->class name) "dir")
	 (dir-name (name-trim-class name)))
	(else #f)))

(request-handler (tmfs-permission? name type)
  (cond ((== (name->class name) "file")
	 (let* ((file (name->file name))
		(user (current-user)))
	   (file-allow? file user type)))
	((== (name->class name) "file-info")
	 (with name* (string-append "file/" (name-trim-class name))
	   (and-with s (tmfs-permission? name* 'read)
	     (== type 'read))))
	((== (name->class name) "dir")
	 (and (!= (name-trim-class name) "")
	      (== type 'read)))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-property-types)
  (and-with user (current-user)
    (let* ((r (property-query `(owner :f ,user) `(:t :f :y)))
	   (t (map (lambda (l) (assoc-ref l :t)) r)))
      (list-remove-duplicates t))))

(request-handler (tmfs-get-property-types name)
  (when (in? (name->class name) '("file" "file-info"))
    (let* ((file (name->file name))
	   (user (current-user)))
      (when (file-allow? file user 'read)
	(let* ((r (property-query `(:t ,file :v)))
	       (t (map (lambda (l) (assoc-ref l :t)) r)))
	  (list-remove-duplicates t))))))

(request-handler (tmfs-get-properties name type)
  (when (in? (name->class name) '("file" "file-info"))
    (let* ((file (name->file name))
	   (user (current-user)))
      (when (file-allow? file user 'read)
	(file-get-properties file type)))))

(request-handler (tmfs-set-properties name type . vals)
  (when (in? (name->class name) '("file" "file-info"))
    (let* ((file (name->file name))
	   (user (current-user)))
      (when (file-allow? file user 'owner)
	(apply file-set-properties (cons* file type vals))
	#t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classifiers (for projects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (tmfs-new-classifier type val)
  (and-let* ((file (create-unique-id))
	     (name (string-append (string-upcase-first type) " - " val))
	     (user (current-user))
	     (ok (null? (file-get-properties file 'owner))))
    (file-set-properties file 'owner user)
    (file-set-properties file 'name name)
    (file-set-properties file 'type "classifier")
    (file-set-properties file 'classify-type type)
    (file-set-properties file 'classify-value val)
    (file-set-properties file 'date (number->string (current-time)))
    (file->url file)))

(request-handler (tmfs-classifiers type)
  (and-with user (current-user)
    (let* ((r (property-query `(classify-type :f ,type)
			      `(type :f "classifier")
			      `(classify-value :f :v)))
	   (f (map (lambda (l) (cons (assoc-ref l :v) (assoc-ref l :f))) r))
	   (c (list-filter f (lambda (x) (file-allow? (cdr x) user 'read)))))
      (list-remove-duplicates c))))

(request-handler (tmfs-project-search-name name search)
  (and-let* ((file (name->file name))
	     (prjs (file-get-properties file 'project))
	     (prj (and (nnull? prjs) (car prjs)))
	     (r (property-query `(name :f ,search) `(project :f ,prj)))
	     (f (map (lambda (l) (assoc-ref l :f)) r)))
    (and (nnull? f) (file->url (car f)))))
