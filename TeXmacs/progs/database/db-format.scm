
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
  ((* "type") :identity)
  ((* "location") :identity)
  ((* "dir") :identity)
  ((* "date") :identity)
  ((* "id") :identity)
  ((* "owner") :users)
  ((* "readable") :users)
  ((* "writable") :users))

(smart-table db-encoder-table
  ;; The routine being used for encoding a field value as a string
  (,:identity ,identity))

(smart-table db-decoder-table
  ;; The routine being used for decoding a field value from a string
  (,:identity ,identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of lists of users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-encode-user user)
  (if (== user "all") user
      (with l (db-search (list (list "type" "user") (list "id" user)))
        (and (pair? l) (car l)))))

(define (db-encode-users users)
  ;;(display* "Encode users " users "\n")
  (list-filter (map db-encode-user users) identity))

(define (db-decode-user id)
  (if (== id "all") id
      (db-get-field-first id "id" #f)))

(define (db-decode-users ids)
  ;;(display* "Decode users " ids "\n")
  (list-filter (map db-decode-user ids) identity))

(smart-table db-encoder-table
  (,:users ,db-encode-users))

(smart-table db-decoder-table
  (,:users ,db-decode-users))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of TeXmacs snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-encode-texmacs vals)
  ;;(display* "Encode TeXmacs " vals "\n")
  (map (cut convert <> "texmacs-stree" "texmacs-snippet") vals))

(define (db-decode-texmacs-one val)
  (with r (convert val "texmacs-snippet" "texmacs-stree")
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

(define (get-encoder enc)
  (or (smart-ref db-encoder-table enc) identity))

(define (get-decoder enc)
  (or (smart-ref db-decoder-table enc) identity))

(define (db-encode-values type attr vals)
  ;; NOTE: patterns (* attr) always take precedence over (type *)
  (cond ((smart-ref db-encoding-table (list type attr)) =>
         (lambda (enc) ((get-encoder enc) vals)))
        ((smart-ref db-encoding-table (list '* attr)) =>
         (lambda (enc) ((get-encoder enc) vals)))
        ((smart-ref db-encoding-table (list type '*)) =>
         (lambda (enc) ((get-encoder enc) vals)))
        (else vals)))

(define (db-decode-values type attr vals)
  ;; NOTE: patterns (* attr) always take precedence over (type *)
  (cond ((smart-ref db-encoding-table (list type attr)) =>
         (lambda (enc) ((get-decoder enc) vals)))
        ((smart-ref db-encoding-table (list '* attr)) =>
         (lambda (enc) ((get-decoder enc) vals)))
        ((smart-ref db-encoding-table (list type '*)) =>
         (lambda (enc) ((get-decoder enc) vals)))
        (else vals)))

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

(tm-define db-preserve? #f)

(tm-define-macro (with-transcode on? . body)
  `(with-global db-preserve? (not ,on?) ,@body))

(tm-define (db-get-field id attr)
  (if db-preserve?
      (former id attr)
      (with-transcode #f
        (let* ((vals (former id attr))
               (type (db-get-field id "type")))
          (db-decode-values type attr vals)))))

(tm-define (db-set-field id attr vals)
  (if db-preserve?
      (former id attr vals)
      (with-transcode #f
        (with type (db-get-field id "type")
          (former id attr (db-encode-values type attr vals))))))

(tm-define (db-get-entry id)
  (if db-preserve?
      (former id)
      (with-transcode #f
        (db-decode-entry (former id)))))

(define (db-preserve-reserved id props)
  (with old-props (db-get-entry id)
    (for (attr (db-reserved-attributes))
      (with old-vals (assoc-ref old-props attr)
        (if old-vals
            (set! props (assoc-set! props attr old-vals))
            (set! props (assoc-remove! props attr))))))
  props)

(tm-define (db-set-entry id l)
  (if db-preserve?
      (former id l)
      (begin
        (set! l (db-preserve-reserved id l))
        (with-transcode #f
          (former id (db-encode-entry l))))))

(define (db-encode-constraint type c)
  (with (attr . vals) c
    (with enc (lambda (val) (car (db-encode-values type attr (list val))))
      (cons attr (map enc vals)))))

(tm-define (db-search l)
  (if db-preserve?
      (former l)
      (with-transcode #f
        (let* ((types (assoc-ref l "type"))
               (type (and (pair? types) (car types)))
               (enc (cut db-encode-constraint type <>)))
          (former (map enc l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access rights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-set-user-info uid id fullname email)
  (with-transcode #f
    (db-set-field uid "id" (list id))
    (db-set-field uid "full-name" (list fullname))
    (db-set-field uid "type" (list "user"))
    (db-set-field uid "owner" (list uid))
    (db-set-field uid "email" (list email))
    (with home (string-append "~" id)
      (when (null? (db-search (list (list "name" home)
                                    (list "type" "dir"))))
        (db-create home "dir" uid)))))

(define (db-allow-many? ids rdone uid udone attr)
  (and (nnull? ids)
       (or (db-allow-one? (car ids) rdone uid udone attr)
           (db-allow-many? (cdr ids) rdone uid udone attr))))

(define (db-allow-groups? id rdone uids udone attr)
  (and (nnull? uids)
       (or (db-allow-one? id rdone (car uids) udone attr)
           (db-allow-groups? id rdone (cdr uids) udone attr))))

(define (db-allow-one? id rdone uid udone attr)
  ;;(display* "Allow one " id ", " uid ", " attr "\n")
  (and (not (in? id rdone))
       (not (in? uid udone))
       (or (== id uid)
           (== id "all")
           (with ids (append (db-get-field id attr)
                             (db-get-field id "owner"))
             (set! ids (list-remove-duplicates ids))
             (set! ids (list-difference ids (cons id rdone)))
             (db-allow-many? ids (cons id rdone) uid udone attr))
           (with grs (db-get-field uid "member")
             (db-allow-groups? id rdone grs (cons uid udone) attr)))))

(tm-define (db-allow? id uid attr)
  (with-transcode #f
    ;;(display* "Allow " id ", " uid ", " attr "\n")
    (db-allow-one? id (list) uid (list) attr)))
