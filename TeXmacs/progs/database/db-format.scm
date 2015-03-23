
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
;; User interface for changing properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-reserved-attributes)
  (list "type" "location" "dir" "date" "id"))

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

(smart-table db-kind-table)
(smart-table db-format-table)

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
