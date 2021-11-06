
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

(define-public tmfs-handler-table (make-ahash-table))

(define-public (object->tmstring s)
 ;; S7 impose an upper bound on the lenght of sequences to be printed
 ;; we override it...
 ;; FIXME: do we have a more elegant way to do it??
 (let-temporarily (((*s7* 'print-length) 9223372036854775807)) (unescape-guile (object->string s))))
 
(define (tmstring->object s) (string->object s))

(define-public (tmfs-handler class action handle)
  ;;(display* "Define " class " :: " action "\n")
  (ahash-set! tmfs-handler-table (cons class action) handle))

(define-public (tmfs-decompose-name name)
  (if (url? name) (set! name (url->unix name)))
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
               (if (string? r) r (object->tmstring r)))))
          ((ahash-ref tmfs-handler-table (cons #t 'load)) =>
           (lambda (handler)
             (with r (handler name)
               (if (string? r) r (object->tmstring r)))))
          (else ""))))

(define-public (tmfs-save u what)
  "Save string @what to url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'save)) =>
           (lambda (handler) (handler name (tmstring->object what))))
          (else ((ahash-ref tmfs-handler-table (cons #t 'save)) u what)))))

(define-public (tmfs-autosave u suf)
  "Autosave name for url @u with suffix @suf on TeXmacs file system, or @#f."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'autosave)) =>
           (lambda (handler) (handler name suf)))
          (else ((ahash-ref tmfs-handler-table (cons #t 'autosave)) u suf)))))

(define-public (tmfs-can-autosave? u)
  (not (not (tmfs-autosave u "~"))))

(define-public (tmfs-remove u)
  "Remove url @u from TeXmacs file system and return @#t on success."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'remove)) =>
           (lambda (handler) (handler name)))
          (else ((ahash-ref tmfs-handler-table (cons #t 'remove)) u)))))

(define-public (tmfs-wrap u)
  "Underlying wrapped url for url @u on TeXmacs file system, or @#f."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'wrap)) =>
           (lambda (handler) (handler name)))
          (else ((ahash-ref tmfs-handler-table (cons #t 'wrap)) u)))))

(define-public (tmfs-date u)
  "Get last modification date for url @u on TeXmacs file system, or @#f."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'date)) =>
           (lambda (handler) (handler name)))
          (else ((ahash-ref tmfs-handler-table (cons #t 'date)) u)))))

(define-public (tmfs-title u doc)
  "Get a nice title for url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((ahash-ref tmfs-handler-table (cons class 'title)) =>
           (lambda (handler) (handler name doc)))
          ((ahash-ref tmfs-handler-table (cons class 'load))
           (if (url? u) (url->system u) u))
          (else ((ahash-ref tmfs-handler-table (cons #t 'title)) u doc)))))

(define-public (tmfs-permission? u type)
  "Check whether we have the permission of a given @type for the url @u."
  (with (class name) (tmfs-decompose-name u)
    (lazy-tmfs-force class)
    (cond ((and (string-ends? (url->unix u) "~")
                (not (tmfs-autosave (url-unglue u 1) "~"))) #f)
          ((and (string-ends? (url->unix u) "#")
                (not (tmfs-autosave (url-unglue u 1) "#"))) #f)
          ((ahash-ref tmfs-handler-table (cons class 'permission?)) =>
           (lambda (handler) (handler name type)))
          ((tmfs-wrap u)
           ((ahash-ref tmfs-handler-table (cons #t 'permission?)) u type))
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

; FIXME: these two procedures should be more general and in the glue
(define (escape-entry s)
  (if (string? s) (string-replace s ":" "%3A") ""))

(define (unescape-entry s)
  (if (string? s) (string-replace s "%3A" ":") ""))

(define (pair->entry p)
  (string-append (escape-entry (car p)) "=" (escape-entry (cdr p))))

(define-public (list->query l)
  (with r (map pair->entry l)
    (string-recompose r "&")))

(define (entry->pair e)
  (with l (string-tokenize-by-char-n e #\= 1)
    (cond ((== (length l) 0) (cons "" ""))
          ((== (length l) 1) (cons (unescape-entry (car l)) ""))
          (else (cons (unescape-entry (car l)) (unescape-entry (cadr l)))))))

(define-public (query->list q)
  (with l (string-tokenize-by-char q #\&)
    (map entry->pair l)))

(define-public (query-ref q var)
  (tmstring->string (or (assoc-ref (query->list q) var) "")))

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

(define-public (tmfs->list s)
  (if (not (tmfs-pair? s)) (list s)
      (cons (tmfs-car s) (tmfs->list (tmfs-cdr s)))))

(define-public (list->tmfs l)
  (apply string-append (list-intersperse l "/")))

(define-public (strip-colon s)
  (if (and (>= (string-length s) 3)
           (string-alpha? (substring s 0 1))
           (== (substring s 1 3) ":/"))
      (string-append (substring s 0 1)
                     (substring s 2 (string-length s)))
      s))

(define-public (url->tmfs-string u)
  (if (and (url-descends? u (get-texmacs-path))
           (!= (url->url u) (get-texmacs-path)))
      (with base (url-append (get-texmacs-path) "x")
        (string-append "tm/" (url->unix (url-delta base u))))
      (let* ((protocol (url-root u))
             (file (url->unix (url-unroot u))))
        (cond ((== protocol "")
               (string-append "here/" file))
              ((== protocol "default")
               (if (os-mingw?)
                   (string-append "file/" (strip-colon file))
                   (string-append "file/" file)))
              (else (string-append protocol "/" file))))))

(define-public (tmfs-string->url s)
  (if (not (tmfs-pair? s))
      (unix->url s)
      (let* ((protocol (tmfs-car s))
             (file (unix->url (tmfs-cdr s))))
        (cond ((== protocol "tm") (url-append (get-texmacs-path) file))
              ((== protocol "here") file)
              ((== protocol "file")
               (if (os-mingw?)
                   (string->url (string-append "/" (tmfs-cdr s)))
                   (url-append (root->url "default") file)))
              ((in? protocol '("http" "https" "ftp" "tmfs"))
               (url-append (root->url protocol) file))
              (else (url-append (root->url "default") s))))))

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

(define-public-macro (tmfs-autosave-handler head . body)
  (with (type what suf) head
    `(tmfs-handler ,(symbol->string type) 'autosave
                   (lambda (,what ,suf) ,@body))))

(define-public-macro (tmfs-remove-handler head . body)
  (with (type what) head
    `(tmfs-handler ,(symbol->string type) 'remove
                   (lambda (,what) ,@body))))

(define-public-macro (tmfs-wrap-handler head . body)
  (with (type what) head
    `(tmfs-handler ,(symbol->string type) 'wrap
                   (lambda (,what) ,@body))))

(define-public-macro (tmfs-date-handler head . body)
  (with (type what) head
    `(tmfs-handler ,(symbol->string type) 'date
                   (lambda (,what) ,@body))))

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
;; Default handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-handler #t 'load
  (lambda (name)
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "generic"))
       (body (document "Invalid tmfs document.")))))

(tmfs-handler #t 'save
  (lambda (name doc) (noop)))

(tmfs-handler #t 'autosave
  (lambda (name suf)
    (and-with u (tmfs-wrap name)
      (and (url-autosave u suf)
           (url-glue name suf)))))

(tmfs-handler #t 'remove
  (lambda (name)
    (and-with u (tmfs-wrap name)
      (url-remove u))))

(tmfs-handler #t 'wrap
  (lambda (name) #f))

(tmfs-handler #t 'date
  (lambda (name)
    (and-with u (tmfs-wrap name)
      (url-last-modified u))))

(tmfs-handler #t 'title
  (lambda (name doc) name))

(tmfs-handler #t 'permission?
  (lambda (name kind)
    (with u (tmfs-wrap name)
      (cond ((not u) (== kind "read"))
            ((== kind "read") (url-test? u "r"))
            ((== kind "write") (url-test? u "w"))
            (else #f)))))

(tmfs-handler #t 'master
  (lambda (name) name))

(tmfs-handler #t 'format
  (lambda (name) "stm"))

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
      (unix->url (string-append "tmfs://aux/" name))))

(define-public (aux-name aux)
  (unix->url (string-append "tmfs://aux/" aux)))

(define-public (aux-set-document aux doc)
  (with name (aux-name aux)
    (buffer-set name doc)
    (ahash-set! aux-buffers aux (buffer-get name))))

(define-public (aux-set-master aux master)
  (with name (aux-name aux)
    (buffer-set-master name master)    
    (ahash-set! aux-masters aux (buffer-get-master name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importation of files using a different format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tmfs-document t)
  (with doc (tm->stree t)
    (if (and (tm-func? doc 'document)
             (not (tm-func? (tm-ref doc 0) 'TeXmacs)))
        `(document (TeXmacs ,(texmacs-version)) ,@(cdr doc))
        doc)))

(tmfs-load-handler (import name)
  (if (and (tmfs-pair? name) (tmfs-pair? (tmfs-cdr name)))
      (let* ((fm (tmfs-car name))
             (u (tmfs-string->url (tmfs-cdr name))))
        (tmfs-document (tree-import u fm)))
      `(document
         (TeXmacs ,(texmacs-version))
         (style (tuple "generic"))
         (body (document "")))))

(tmfs-title-handler (import name doc)
  (if (and (tmfs-pair? name) (tmfs-pair? (tmfs-cdr name)))
      (let* ((fm (tmfs-car name))
             (u (tmfs-string->url (tmfs-cdr name)))
             (last (url->system (url-tail u))))
        (string-append last " - " (upcase-first fm)))
      (url-tail name)))
