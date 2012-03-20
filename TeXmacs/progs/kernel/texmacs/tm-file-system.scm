
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-file-system.scm
;; DESCRIPTION : The TeXmacs file system
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-file-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public lazy-tmfs-table (make-ahash-table))

(define-public-macro (lazy-tmfs-handler module . classes)
  `(for-each (lambda (class) (ahash-set! lazy-tmfs-table class ',module))
             ',classes))

(define-public (lazy-tmfs-force class)
  (if (string? class) (set! class (string->symbol class)))
  (and-with module (ahash-ref lazy-tmfs-table class)
    (ahash-remove! lazy-tmfs-table class)
    (eval `(use-modules ,module))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handler system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmfs-handler-table (make-ahash-table))

(define-public (tmfs-handler class action handle)
  (ahash-set! tmfs-handler-table (cons class action) handle))

(define-public (tmfs-decompose-name name)
  (if (url? name) (set! name (url->string name)))
  (if (string-starts? name "tmfs://") (set! name (string-drop name 7)))
  (with i (string-index name #\/)
    (list (if i (substring name 0 i) "file")
	  (if i (substring name (+ i 1) (string-length name)) name))))

(define-public (tmfs-load u)
  "Load url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'load)) =>
	   (lambda (handler) (object->string (handler name))))
	  (else ((ahash-ref tmfs-handler-table (cons #t 'load)) u)))))

(define-public (tmfs-save u what)
  "Save string @what to url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'save)) =>
	   (lambda (handler) (handler name (string->object what))))
	  (else ((ahash-ref tmfs-handler-table (cons #t 'save)) u what)))))

(define-public (tmfs-title u doc)
  "Get a nice title for url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'title)) =>
	   (lambda (handler) (handler name doc)))
	  ((ahash-ref tmfs-handler-table (cons class 'load))
	   (if (url? u) (url->string u) u))
	  (else ((ahash-ref tmfs-handler-table (cons #t 'title)) u doc)))))

(define-public (tmfs-permission? u type)
  "Check whether we have the permission of a given @type for the url @u."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((string-ends? (url->string u) "~") #f)
	  ((string-ends? (url->string u) "#") #f)
	  ((ahash-ref tmfs-handler-table (cons class 'permission?)) =>
	   (lambda (handler) (handler name type)))
	  ((ahash-ref tmfs-handler-table (cons class 'load))
	   (== type "read"))
	  (else
	   ((ahash-ref tmfs-handler-table (cons #t 'permission?)) u type)))))

(define-public (tmfs-remote? u)
  "Check whether the url @u is handled remotedly."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (not (ahash-ref tmfs-handler-table (cons class 'load)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for making and decomposing queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pair->entry p)
  (string-append (car p) "=" (cdr p)))

(define-public (list->query l)
  (with r (map pair->entry l)
    (string-recompose r "&")))

(define (entry->pair e)
  (with l (string-tokenize-by-char-n e #\= 1)
    (cond ((== (length l) 0) (cons "" ""))
          ((== (length l) 1) (cons (car l) ""))
          (else (cons (car l) (cadr l))))))

(define-public (query->list q)
  (with l (string-tokenize-by-char q #\&)
    (map entry->pair l)))

(define-public (query-ref q var)
  (or (assoc-ref (query->list q) var) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros for defining handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (tmfs-load-handler head . body)
  (with (type what) head
    `(tmfs-handler ,(symbol->string type) 'load
                   (lambda (,what) ,@body))))

(define-public-macro (tmfs-save-handler head . body)
  (with (type what doc) head
    `(tmfs-handler ,(symbol->string type) 'save
                   (lambda (,what ,doc) ,@body))))

(define-public-macro (tmfs-title-handler head . body)
  (with (type what doc) head
    `(tmfs-handler ,(symbol->string type) 'title
                   (lambda (,what ,doc) ,@body))))

(define-public-macro (tmfs-permission-handler head . body)
  (with (type what kind) head
    `(tmfs-handler ,(symbol->string type) 'permission?
                   (lambda (,what ,kind) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(tmfs-load-handler (id what)
;;  `(document
;;     (TeXmacs "1.0.6.3")
;;     (style (tuple "generic"))
;;     (body (document ,what))))
