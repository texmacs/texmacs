
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
  (list "type" "location" "dir" "date" "id"))

(smart-table db-encoding-table
  ;; For each entry+field type, specify the encoding being used for
  ;; the field value.  This allows for instance to use TeXmacs snippets
  ;; instead of plain string values.
  ((* "owner") :users)
  ((* "readable") :users)
  ((* "writable") :users))

(smart-table db-encoder-table
  ;; The routine being used for encoding a field value as a string
  )

(smart-table db-decoder-table
  ;; The routine being used for decoding a field value from a string
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of lists of users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-encoder-table
  (:users ,db-encode-users))

(smart-table db-decoder-table
  (:users ,db-decode-users))

(define (db-encode-user user)
  (if (== user "all") user
      (with l (db-search (list (list "type" "user") (list "id" user)))
        (and (pair? l) (car l)))))

(define (db-encode-users users)
  (list-filter (map db-encode-user users) identity))

(define (db-decode-user id)
  (if (== id "all") id
      (db-get-field-first id "id" #f)))

(define (db-decode-users ids)
  (list-filter (map db-decode-user ids) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of TeXmacs snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-encoder-table
  (:texmacs ,db-encode-texmacs))

(smart-table db-decoder-table
  (:texmacs ,db-decode-texmacs))

(define (db-encode-texmacs vals)
  (map (cut convert <> "texmacs-stree" "texmacs-snippet") vals))

(define (db-decode-texmacs-one val)
  (with r (convert val "texmacs-snippet" "texmacs-stree")
    (if (tm-func? r 'document 1) (tm-ref r 0) r)))

(define (db-decode-texmacs vals)
  (map db-decode-texmacs-one vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic encoding and decoding of field values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-encode-value type attr val)
  ;; NOTE: patterns (* attr) always take precedence over (type *)
  (cond ((smart-ref db-encoding-table (list type attr)) =>
         (lambda (enc) ((smart-ref db-encoder-table enc) val)))
        ((smart-ref db-encoding-table (list '* attr)) =>
         (lambda (enc) ((smart-ref db-encoder-table enc) val)))
        ((smart-ref db-encoding-table (list type '*)) =>
         (lambda (enc) ((smart-ref db-encoder-table enc) val)))
        (else val)))

(define (db-decode-value type attr val)
  ;; NOTE: patterns (* attr) always take precedence over (type *)
  (cond ((smart-ref db-encoding-table (list type attr)) =>
         (lambda (enc) ((smart-ref db-decoder-table enc) val)))
        ((smart-ref db-encoding-table (list '* attr)) =>
         (lambda (enc) ((smart-ref db-decoder-table enc) val)))
        ((smart-ref db-encoding-table (list type '*)) =>
         (lambda (enc) ((smart-ref db-decoder-table enc) val)))
        (else val)))

(define (db-encode-entry l)
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    (map (lambda (f) (cons (car f) (db-encode-value type (car f) (cdr f))))
         l)))

(define (db-decode-entry l)
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    (map (lambda (f) (cons (car f) (db-decode-value type (car f) (cdr f))))
         l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for changing properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-all id)
  (with t (make-ahash-table)
    (for (attr (db-get-attributes id))
      (ahash-set! t attr (db-get-field id attr)))
    (ahash-table->list t)))

(tm-define (db-set-all id props)
  (let* ((old (db-get-attributes id))
         (new (map car props))
         (del (list-difference old (append new (db-reserved-attributes)))))
    (for (attr del)
      (db-remove-field id attr)))
  (for (prop props)
    (when (and (pair? prop) (list? (cdr prop))
               (nin? (car prop) (db-reserved-attributes))
               (or (!= (car prop) "owner") (nnull? (cdr prop))))
      (db-set-field id (car prop) (cdr prop)))))

(define (user-decode id)
  (if (== id "all") id
      (db-get-field-first id "id" #f)))

(define (user-encode user)
  (if (== user "all") user
      (with l (db-search (list (list "type" "user") (list "id" user)))
        (and (pair? l) (car l)))))

(define (prop-decode x)
  (with (attr . vals) x
    (if (nin? attr '("owner" "readable" "writable")) x
        (cons attr (list-difference (map user-decode vals) (list #f))))))

(define (prop-encode x)
  (with (attr . vals) x
    (if (nin? attr '("owner" "readable" "writable")) x
        (cons attr (list-difference (map user-encode vals) (list #f))))))

(tm-define (db-properties-decode l)
  ;;(display* "decode " l " -> " (map prop-decode l) "\n")
  (map prop-decode l))

(tm-define (db-properties-encode l)
  ;;(display* "encode " l " -> " (map prop-encode l) "\n")
  (map prop-encode l))

(define (first-leq? p1 p2)
  (string<=? (car p1) (car p2)))

(tm-define (db-get-all-decoded id)
  (with raw-props (sort (db-get-all id) first-leq?)
    (db-decode-fields (db-properties-decode raw-props))))

(tm-define (db-set-all-encoded id props*)
  (with props (db-encode-fields props*)
    (with raw-props (db-properties-encode props)
      (db-set-all id raw-props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of fields as a function of type and property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-encode-field type var val)
  ;;(display* "  Encode " val "\n")
  (cond ((in? var (db-reserved-attributes)) val)
        ((smart-ref db-format-table type)
         (convert val "texmacs-stree" "texmacs-snippet"))
        ((string? val) val)
        (else "")))

(tm-define (db-decode-field type var val)
  ;;(display* "  Decode " val "\n")
  (cond ((in? var (db-reserved-attributes)) val)
        ((smart-ref db-format-table type)
         (with r (convert val "texmacs-snippet" "texmacs-stree")
           (if (tm-func? r 'document 1) (tm-ref r 0) r)))
        (else val)))

(tm-define (db-encode-fields l)
  ;;(display* "  Encoding fields " l "\n")
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    ;;(display* "    type= " type "\n")
    (with cv (lambda (f)
               (cons (car f)
                     (map (cut db-encode-field type (car f) <>) (cdr f))))
      (if type (map cv l) l))))

(tm-define (db-decode-fields l)
  ;;(display* "  Decoding fields " l "\n")
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    ;;(display* "    type= " type "\n")
    (with cv (lambda (f)
               (cons (car f)
                     (map (cut db-decode-field type (car f) <>) (cdr f))))
      (if type (map cv l) l))))
