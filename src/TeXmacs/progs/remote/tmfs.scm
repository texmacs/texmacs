
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs.scm
;; DESCRIPTION : remote TeXmacs file systems
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs)
  (:use (remote client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfs-server)
  (or (logged-server) (get-server) (default-server)))

(tm-define (remote-new-file)
  (with-server (tmfs-server)
    (with nr (remote-request `(file-new))
      (when nr
	(new-buffer)
	(set-name-buffer
	  (string-append "tmfs://" (number->string nr) ".scm"))))))

(define (remote-file name)
  (if (string-starts? name "tmfs://")
      (set! name (string-drop name 7)))
  (and (string-ends? name ".scm")
       (string->number (string-drop-right name 4))))

(tm-define (remote-load name)
  (with-server (tmfs-server)
    (and-with file (remote-file name)
      (remote-request `(file-get ,file)))))

(tm-define (remote-save name what)
  (with-server (tmfs-server)
    (and-with file (remote-file name)
      (remote-request `(file-set ,file ,what)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-property-query . patterns)
  (with-server (tmfs-server)
    (remote-request `(property-query ,@patterns))))

(tm-define (remote-property-solutions pattern)
  (with-server (tmfs-server)
    (remote-request `(property-solutions ,pattern))))

(tm-define (remote-property-set property)
  (with-server (tmfs-server)
    (remote-request `(property-add ,property))))

(tm-define (remote-property-remove pattern)
  (with-server (tmfs-server)
    (remote-request `(property-remove ,pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique properties of the current document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-buffer?)
  (with u (get-name-buffer)
    (and (not (url-none? u))
	 (string-starts? (url->string u) "tmfs://"))))

(tm-define (remote-get-property prop)
  (:synopsis "Get the remote property @prop for the current buffer.")
  (:argument prop "Property")
  (and-let* ((file (remote-file (url->string (get-name-buffer))))
	     (type (and (string? prop) (string->symbol prop)))
	     (sols (remote-property-query `(,type ,file :x)))
	     (r (map cdar sols)))
    (and (nnull? r) (string-recompose-comma r))))

(tm-define (remote-set-property prop val)
  (:synopsis "Set the remote property @prop for the current buffer to @val.")
  (:argument prop "Property")
  (:argument val "Value")
  (and-let* ((file (remote-file (url->string (get-name-buffer))))
	     (type (and (string? prop) (string->symbol prop)))
	     (vals (string-tokenize-comma val)))
    (remote-property-remove `(,type ,file :x))
    (for-each (lambda (v) (remote-property-set `(,type ,file ,v))) vals)))

(tm-define (interactive-remote-set-property prop)
  (:interactive #t)
  (interactive (lambda (val) (remote-set-property prop val))
    (list prop "string" (remote-get-property prop))))

(tm-define (interactive-remote-set-property-and-value)
  (:interactive #t)
  (interactive (lambda (prop) (interactive-remote-set-property prop))
    (list "Property" "string")))
