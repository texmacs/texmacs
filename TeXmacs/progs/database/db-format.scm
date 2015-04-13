
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-format.scm
;; DESCRIPTION : Specific entry and field semantics for TeXmacs databases
;;               such as the way fields are encoded
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-format)
  (:use (database db-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The current encoding scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define db-encoding :default)

(tm-define-macro (with-encoding enc . body)
  `(with-global db-encoding ,enc ,@body))

(tm-define (db-reset)
  (former)
  (set! db-encoding :default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-kind-table
  ;; For various kinds of databases (bibliographies, address books, etc.),
  ;; specify the list of admissible entry types.
  )

(smart-table db-format-table
  ;; For each entry type, specify the mandatory, alternative and
  ;; optional fields.
  )

(tm-define (db-reserved-attributes)
  (list "type" "location" "dir" "date" "pseudo" "id"))

(tm-define (db-meta-attributes)
  (list "date" "contributor" "modus" "origin" "newer"))

(smart-table db-encoding-table
  ;; For each entry+field type, specify the encoding being used for
  ;; the field value.  This allows for instance to use TeXmacs snippets
  ;; instead of plain string values.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (format->attributes fm)
  (cond ((string? fm) (list fm))
        ((or (func? fm 'and) (func? fm 'or) (func? fm 'optional))
         (append-map format->attributes (cdr fm)))
        (else (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of TeXmacs snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-encode-texmacs-one val)
  (serialize-texmacs-snippet (stree->tree val)))

(define (db-encode-texmacs vals)
  ;;(display* "Encode TeXmacs " vals "\n")
  (map db-encode-texmacs-one vals))

(define (db-decode-texmacs-one val)
  (with r (tree->stree (parse-texmacs-snippet val))
    (if (tm-func? r 'document 1) (tm-ref r 0) r)))

(define (db-decode-texmacs vals)
  ;;(display* "Decode TeXmacs " vals "\n")
  (map db-decode-texmacs-one vals))

(smart-table db-encoder-table
  (,:texmacs ,db-encode-texmacs))

(smart-table db-decoder-table
  (,:texmacs ,db-decode-texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic encoding and decoding of field values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-get-encoding type attr)
  (or (smart-ref db-encoding-table (list attr type db-encoding))
      (smart-ref db-encoding-table (list attr type '*))
      (smart-ref db-encoding-table (list attr '* db-encoding))
      (smart-ref db-encoding-table (list attr '* '*))
      (smart-ref db-encoding-table (list '* type db-encoding))
      (smart-ref db-encoding-table (list '* type '*))
      (smart-ref db-encoding-table (list '* '* db-encoding))
      (smart-ref db-encoding-table (list '* '* '*))
      :texmacs))

(define (db-encode-values type attr vals)
  (let* ((enc (db-get-encoding type attr))
         (cv (or (smart-ref db-encoder-table enc) identity)))
    (cv vals)))

(define (db-decode-values type attr vals)
  (let* ((enc (db-get-encoding type attr))
         (cv (or (smart-ref db-decoder-table enc) identity)))
    (cv vals)))

(tm-define (db-encode-field type f)
  (cons (car f) (db-encode-values type (car f) (cdr f))))

(tm-define (db-decode-field type f)
  (cons (car f) (db-decode-values type (car f) (cdr f))))

(tm-define (db-encode-entry l)
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    (map (cut db-encode-field type <>) l)))

(tm-define (db-decode-entry l)
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    (map (cut db-decode-field type <>) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrap basic interface to databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-field id attr)
  (if (not db-encoding)
      (former id attr)
      (let* ((vals (with-encoding #f (former id attr)))
             (type (with-encoding #f (db-get-field id "type"))))
        (db-decode-values type attr vals))))

(tm-define (db-set-field id attr vals)
  (if (not db-encoding)
      (former id attr vals)
      (let* ((type (with-encoding #f (db-get-field id "type")))
             (vals (db-encode-values type attr vals)))
        (with-encoding #f (former id attr vals)))))

(tm-define (db-get-entry id)
  (if (not db-encoding)
      (former id)
      (let* ((l (db-decode-entry (with-encoding #f (former id))))
             (attrs (map car l))
             (types (assoc-ref l "type"))
             (type (and (pair? types) (car types)))
             (fm (smart-ref db-format-table type))
             (std-attrs (format->attributes fm))
             (a1 (list-filter std-attrs (cut in? <> attrs)))
             (a2 (list-filter attrs (cut nin? <> std-attrs)))
             (get (lambda (a) (cons a (assoc-ref l a)))))
        (append (map get a1) (map get a2)))))

(tm-define (db-set-entry id l)
  (if (not db-encoding)
      (former id l)
      (with el (db-encode-entry l)
        (with-encoding #f (former id el)))))

(tm-define (db-remove-entry id)
  (if (not db-encoding)
      (former id)
      (with-encoding #f (former id))))

(define (db-encode-constraint type c)
  (with (attr . vals) c
    (with enc (lambda (val) (car (db-encode-values type attr (list val))))
      (cons attr (map enc vals)))))

(tm-define (db-search l)
  (if (not db-encoding)
      (former l)
      (let* ((types (assoc-ref l "type"))
             (type (and (pair? types) (car types)))
             (enc (cut db-encode-constraint type <>))
             (el (map enc l)))
        (with-encoding #f (former el)))))
