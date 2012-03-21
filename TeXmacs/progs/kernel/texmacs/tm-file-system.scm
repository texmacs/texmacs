
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
	   (lambda (handler)
             (with r (handler name)
               (if (string? r) r (object->string r)))))
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

(define-public (tmfs-master u)
  "Get a master url @u for linking and navigation."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'master)) =>
	   (lambda (handler) (handler name)))
	  (else u))))

(define-public (tmfs-format u)
  "Get file format for url @u."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'format)) =>
	   (lambda (handler) (handler name)))
	  (else "stm"))))

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
;; Subroutines for building and analyzing TMFS URLs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tmfs-pair? s)
  (string-index s #\/))

(define-public (tmfs-car s)
  (with i (string-index s #\/)
    (and i (substring s 0 i))))

(define-public (tmfs-cdr s)
  (with i (string-index s #\/)
    (and i (substring s (+ i 1) (string-length s)))))

(define-public (url->tmfs-string u)
  (if (url-descends? u (get-texmacs-path))
      (with base (url-append (get-texmacs-path) "x")
        (string-append "tm/" (url->string (url-delta base u))))
      (let* ((protocol (url-root u))
             (file (url->string (url-unroot u))))
        (cond ((== protocol "") (string-append "here" file))
              ((== protocol "default") (string-append "file/" file))
              (else (string-append protocol "/" file))))))

(define-public (tmfs-string->url s)
  (if (not (tmfs-pair? s))
      (string->url s)
      (let* ((protocol (tmfs-car s))
             (file (string->url (tmfs-cdr s))))
        (cond ((== protocol "tm") (url-append (get-texmacs-path) file))
              ((== protocol "here") file)
              ((== protocol "file") (url-append (root->url "default") file))
              (else (url-append (root->url protocol) file))))))

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

(define-public-macro (tmfs-master-handler head . body)
  (with (type what) head
    `(tmfs-handler ,(symbol->string type) 'master
                   (lambda (,what) ,@body))))

(define-public-macro (tmfs-format-handler head . body)
  (with (type what) head
    `(tmfs-handler ,(symbol->string type) 'format
                   (lambda (,what) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (id what)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic"))
     (body (document ,what))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public aux-buffers (make-ahash-table))
(define-public aux-masters (make-ahash-table))

(tmfs-load-handler (aux name)
  (or (ahash-ref aux-buffers name)
      `(document
         (TeXmacs ,(texmacs-version))
         (style (tuple "generic"))
         (body (document "")))))

(tmfs-title-handler (aux name doc)
  name)

(tmfs-master-handler (aux name)
  (or (ahash-ref aux-masters name)
      (string->url (string-append "tmfs://aux/" name))))

(define-public (aux-name aux)
  (string->url (string-append "tmfs://aux/" aux)))

(define-public (aux-set-document aux doc)
  (with name (aux-name aux)
    (buffer-set name doc)
    (ahash-set! aux-buffers aux (buffer-get name))))

(define-public (aux-set-master aux master)
  (with name (aux-name aux)
    (buffer-set-master name master)    
    (ahash-set! aux-masters aux (buffer-get-master name))))
