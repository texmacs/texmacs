
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : file-system.scm
;; DESCRIPTION : Low level routines for the TeXmacs file system
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tmfs file-system))
(use-modules (tools base) (tools abbrevs) (tools ahash-table)
	     (tools list) (tools file)
	     (server request) (server atoms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfs-dir) (string-append (server-dir) "/tmfs"))
(define (tmfs-files-dir) (string-append (tmfs-dir) "/files"))
(define (tmfs-properties-dir) (string-append (tmfs-dir) "/properties"))

(define (tmfs-initialize)
  (ignore-errors 'system-error (mkdir (tmfs-dir)))
  (ignore-errors 'system-error (mkdir (tmfs-files-dir)))
  (ignore-errors 'system-error (mkdir (tmfs-properties-dir))))

(tmfs-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file->name file)
  (let* ((dir (number->string (remainder file 256)))
	 (full-dir (string-append (tmfs-files-dir) "/" dir))
	 (name (number->string file)))
    (if (not (access? full-dir R_OK)) (mkdir full-dir))
    (string-append full-dir "/" name)))

(define-public (file-new)
  (let* ((old (atom-get-property "admin" 'tmfs "serial"))
	 (new (if old (+ old 1) 0)))
    (atom-set-property "admin" 'tmfs "serial" new)
    new))

(define-public (file-set file s)
  (with name (file->name file)
    (ignore-errors 'system-error (save-string name s))))

(define-public (file-get file)
  (with name (file->name file)
    (ignore-errors 'system-error (load-string name))))

(define-public (file-get-properties file type)
  (with r (property-query `(,type ,file :x))
    (map cdar r)))

(define-public (file-set-properties file type . vals)
  (property-remove `(,type ,file :x))
  (for-each (lambda (val) (property-add `(,type ,file ,val))) vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define property-table (make-ahash-table))

(define (string-drop-right s n)
  (substring s 0 (- (string-length s) n)))

(define (base256->number s)
  (if (== s "") 0
      (+ (* 256 (base256->number (string-drop-right s 1)))
	 (char->integer (string-ref s (- (string-length s) 1))))))

(define (property->name entry)
  (with name (if (== (length entry) 2)
		 (string-append (number->string (car entry)) "-"
				(number->string (cadr entry)))
		 (string-append (symbol->string (car entry)) "-"
				(cadr entry) "-"
				(number->string (caddr entry))))
    (let* ((dir (number->string (remainder (base256->number name) 255)))
	   (full-dir (string-append (tmfs-properties-dir) "/" dir)))
      (if (not (access? full-dir R_OK)) (mkdir full-dir))
      (string-append full-dir "/" name))))

(define (property-list-get entry)
  (if (number? (cadr entry)) (set! entry (cdr entry)))
  (with l (ahash-ref property-table entry)
    (if l l
	(with saved-l (if (access? (property->name entry) R_OK)
			  (load-object (property->name entry))
			  '())
	  (ahash-set! property-table entry saved-l)
	  saved-l))))

(define (property-list-set entry l)
  (if (number? (cadr entry)) (set! entry (cdr entry)))
  ;;(display* "Set " entry " to " l "\n")
  (ahash-set! property-table entry l)
  (save-object (property->name entry) l))

(define (property-positional-change type given pos prop add?)
  (let* ((list-change (if add? list-add list-remove))
	 (entry (list type given pos))
	 (l (property-list-get entry)))
    (property-list-set entry (list-change l prop))))

(define (property-change-sub type l pos prop add?)
  (when (nnull? l)
    (when (or (number? (car l)) (string? (car l)))
      (property-positional-change type (car l) pos prop add?))
    (property-change-sub type (cdr l) (+ pos 1) prop add?)))

(define (property-change prop add?)
  ;;(display* "Change " prop " to " add? "\n")
  (when (and (list? prop) (pair? prop) (symbol? (car prop)))
    (property-change-sub (car prop) (cdr prop) 0 prop add?)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching for properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bindings-substitute x bindings)
  (cond ((assoc-ref bindings x) => identity)
	((list? x) (map (cut bindings-substitute <> bindings) x))
	(else x)))

(define (property-matches? property pattern bindings)
  ;;(display* "Match " property " against " pattern " for " bindings "\n")
  (cond ((keyword? pattern)
	 (with val (assoc-ref bindings pattern)
	   (if val
	       (property-matches? property val bindings)
	       (assoc-set! bindings pattern property))))
	((== property pattern) bindings)
	((and (pair? property) (pair? pattern))
	 (and-with r (property-matches? (car property) (car pattern) bindings)
	   (with new-pattern (bindings-substitute (cdr pattern) bindings)
	     (property-matches? (cdr property) new-pattern r))))
	(else #f)))

(define (property-match-against l pattern bindings)
  (if (null? l) l
      (let* ((match (property-matches? (car l) pattern bindings))
	     (r (property-match-against (cdr l) pattern bindings)))
	(if match (cons match (list-remove r match)) r))))

(define (property-dispatcher l pos)
  (cond ((null? l) #f)
	((or (string? (car l)) (number? (car l))) (list (car l) pos))
	(else (property-dispatcher (cdr l) (+ pos 1)))))

(define (property-match pattern bindings)
  (and-with x (and (pair? pattern) (property-dispatcher (cdr pattern) 0))
    (with (given pos) x
      (with l (property-list-get (list (car pattern) given pos))
	(property-match-against l pattern bindings)))))

(define (property-match-several patterns bindings)
  (if (null? patterns) (list bindings)
      (with r (property-match (car patterns) bindings)
	(append-map
	  (lambda (new-bindings)
	    (with new-patterns
		(bindings-substitute (cdr patterns) new-bindings)
	      (property-match-several new-patterns new-bindings)))
	 r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (property-query . patterns)
  (property-match-several patterns '()))

(define-public (property-solutions pattern)
  (with sols (property-match-several (list pattern) '())
    (map (cut bindings-substitute pattern <>) sols)))

(define-public (property-add property)
  (display* "Add property " property "\n")
  (property-change property #t))

(define-public (property-remove pattern)
  (display* "Remove property " pattern "\n")
  (for-each (cut property-change <> #f)
	    (property-solutions pattern))
  #t)
