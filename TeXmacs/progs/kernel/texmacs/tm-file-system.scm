
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
    (cond ((ahash-ref tmfs-handler-table (cons class 'load)) =>
	   (lambda (handler) (object->string (handler name))))
	  (else ((ahash-ref tmfs-handler-table (cons #t 'load)) u)))))

(define-public (tmfs-save u what)
  "Save string @what to url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (cond ((ahash-ref tmfs-handler-table (cons class 'save)) =>
	   (lambda (handler) (handler name (string->object what))))
	  (else ((ahash-ref tmfs-handler-table (cons #t 'save)) u what)))))

(define-public (tmfs-name u)
  "Get a nice name for url @u on TeXmacs file system."
  (with (class name) (tmfs-decompose-name u)
    (cond ((ahash-ref tmfs-handler-table (cons class 'name)) =>
	   (lambda (handler) (handler name)))
	  ((ahash-ref tmfs-handler-table (cons class 'load))
	   (if (url? u) (url->string u) u))
	  (else ((ahash-ref tmfs-handler-table (cons #t 'name)) u)))))

(define-public (tmfs-permission? u type)
  "Check whether we have the permission of a given @type for the url @u."
  (with (class name) (tmfs-decompose-name u)
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
    (not (ahash-ref tmfs-handler-table (cons class 'load)))))

;(define (id-load what)
;  `(document
;    (TeXmacs "1.0.6.3")
;    (style (tuple "generic"))
;    (body (document ,what))))
;
;(tmfs-handler "id" 'load id-load)
