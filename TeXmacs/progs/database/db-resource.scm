
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-resources.scm
;; DESCRIPTION : TeXmacs databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-resource))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution of SQL commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define current-database (url-none))

(tm-define-macro (with-database db . body)
  `(with-global current-database ,db ,@body))

(tm-define (db-init-database)
  (when (url-none? current-database)
    (texmacs-error "db-init-database" "no database specified"))
  (when (not (url-exists? current-database))
    (display* "Create " current-database "\n")
    (sql-exec current-database
              "CREATE TABLE props (rid text, attr text, val text)")))

(tm-define (db-sql . l)
  (db-init-database)
  ;;(display* (apply string-append l) "\n")
  (display* (url-tail current-database) "] " (apply string-append l) "\n")
  (sql-exec current-database (apply string-append l)))

(tm-define (db-sql* . l)
  (with r (apply db-sql l)
    (with f (lambda (x) (and (pair? x) (car x)))
      (map f (if (null? r) r (cdr r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic ressources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-insert rid attr val)
  (db-sql "INSERT INTO props VALUES (" (sql-quote rid)
          ", " (sql-quote attr)
          ", " (sql-quote val) ")"))

(tm-define (db-remove rid attr val)
  (db-sql "DELETE FROM props WHERE rid=" (sql-quote rid)
          " AND attr=" (sql-quote attr)
          ", AND val=" (sql-quote val)))

(tm-define (db-set rid attr vals)
  (db-reset rid attr)
  (for-each (cut db-insert rid attr <>) vals))

(tm-define (db-reset rid attr)
  (db-sql "DELETE FROM props WHERE rid=" (sql-quote rid)
          " AND attr=" (sql-quote attr)))

(tm-define (db-reset-all rid)
  (db-sql "DELETE FROM props WHERE rid=" (sql-quote rid)))

(tm-define (db-attributes rid)
  (db-sql* "SELECT DISTINCT attr FROM props WHERE rid=" (sql-quote rid)))

(tm-define (db-get rid attr)
  (db-sql* "SELECT DISTINCT val FROM props WHERE rid=" (sql-quote rid)
           " AND attr=" (sql-quote attr)))

(tm-define (db-get-first rid attr default)
  (with l (db-get rid attr)
    (if (null? l) default (car l))))

(tm-define (db-create name type uid)
  (with rid (create-unique-id)
    (if (nnull? (db-get rid "type"))
        (db-create name type uid)
        (begin
          (db-insert rid "name" name)
          (db-insert rid "type" type)
          (db-insert rid "owner" uid)
          rid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching ressources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-search-join l i)
  (with s (string-append "props AS p" (number->string i))
    (if (null? (cdr l)) s
        (string-append s " JOIN " (db-search-join (cdr l) (+ i 1))))))

(define (db-search-on l i)
  (with (attr val) (car l)
    (let* ((pi (string-append "p" (number->string i)))
           (srid (string-append pi ".rid=p1.rid"))
           (sattr (string-append pi ".attr=" (sql-quote attr)))
           (sval (string-append pi ".val=" (sql-quote val)))
           (spair (string-append sattr " AND " sval))
           (q (if (= i 1) spair (string-append srid " AND " spair))))
      (if (null? (cdr l)) q
          (string-append q " AND " (db-search-on (cdr l) (+ i 1)))))))

(tm-define (db-search l)
  (if (null? l)
      (db-sql* "SELECT DISTINCT rid FROM props")
      (let* ((join (db-search-join l 1))
             (on (db-search-on l 1))
             (sep (if (null? (cdr l)) " WHERE " " ON ")))
        (db-sql* "SELECT DISTINCT p1.rid FROM " join sep on))))

(tm-define (db-search-name name)
  (db-search (list (list "name" name))))

(tm-define (db-search-owner owner)
  (db-search (list (list "owner" owner))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access rights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-set-user-info uid id fullname email)
  (db-set uid "id" (list id))
  (db-set uid "full-name" (list fullname))
  (db-set uid "type" (list "user"))
  (db-set uid "owner" (list uid))
  (db-set uid "email" (list email))
  (with home (string-append "~" id)
    (when (null? (db-search (list (list "name" home)
                                        (list "type" "dir"))))
      (db-create home "dir" uid))))

(define (db-allow-many? rids rdone uid udone attr)
  (and (nnull? rids)
       (or (db-allow-one? (car rids) rdone uid udone attr)
           (db-allow-many? (cdr rids) rdone uid udone attr))))

(define (db-allow-groups? rid rdone uids udone attr)
  (and (nnull? uids)
       (or (db-allow-one? rid rdone (car uids) udone attr)
           (db-allow-groups? rid rdone (cdr uids) udone attr))))

(define (db-allow-one? rid rdone uid udone attr)
  ;;(display* "Allow one " rid ", " uid ", " attr "\n")
  (and (not (in? rid rdone))
       (not (in? uid udone))
       (or (== rid uid)
           (== rid "all")
           (with rids (append (db-get rid attr)
                              (db-get rid "owner"))
             (set! rids (list-remove-duplicates rids))
             (set! rids (list-difference rids (cons rid rdone)))
             (db-allow-many? rids (cons rid rdone) uid udone attr))
           (with grs (db-get uid "member")
             (db-allow-groups? rid rdone grs (cons uid udone) attr)))))

(tm-define (db-allow? rid uid attr)
  ;;(display* "Allow " rid ", " uid ", " attr "\n")
  (db-allow-one? rid (list) uid (list) attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for changing properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-reserved-attributes)
  (list "type" "location" "dir" "date" "id"))

(tm-define (db-get-all rid)
  (with t (make-ahash-table)
    (for (attr (db-attributes rid))
      (ahash-set! t attr (db-get rid attr)))
    (ahash-table->list t)))

(tm-define (db-set-all rid props)
  (let* ((old (db-attributes rid))
         (new (map car props))
         (del (list-difference old (append new (db-reserved-attributes)))))
    (for (attr del)
      (db-reset rid attr)))
  (for (prop props)
    (when (and (pair? prop) (list? (cdr prop))
               (nin? (car prop) (db-reserved-attributes))
               (or (!= (car prop) "owner") (nnull? (cdr prop))))
      (db-set rid (car prop) (cdr prop)))))

(define (user-decode rid)
  (if (== rid "all") rid
      (db-get-first rid "id" #f)))

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

(tm-define (db-get-all-decoded rid)
  (with raw-props (sort (db-get-all rid) first-leq?)
    (db-decode-entries (db-properties-decode raw-props))))

(tm-define (db-set-all-encoded rid props*)
  (with props (db-encode-entries props*)
    (with raw-props (db-properties-encode props)
      (db-set-all rid raw-props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database kinds and formats of resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-kind-table)
(smart-table db-format-table)

(tm-define (db-encode-entry type var val)
  ;;(display* "  Encode " val "\n")
  (cond ((in? var (db-reserved-attributes)) val)
        ((smart-ref db-format-table type)
         (convert val "texmacs-stree" "texmacs-snippet"))
        ((string? val) val)
        (else "")))

(tm-define (db-decode-entry type var val)
  ;;(display* "  Decode " val "\n")
  (cond ((in? var (db-reserved-attributes)) val)
        ((smart-ref db-format-table type)
         (with r (convert val "texmacs-snippet" "texmacs-stree")
           (if (tm-func? r 'document 1) (tm-ref r 0) r)))
        (else val)))

(tm-define (db-encode-entries l)
  ;;(display* "  Encoding entries " l "\n")
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    ;;(display* "    type= " type "\n")
    (with cv (lambda (f)
               (cons (car f)
                     (map (cut db-encode-entry type (car f) <>) (cdr f))))
      (if type (map cv l) l))))

(tm-define (db-decode-entries l)
  ;;(display* "  Decoding entries " l "\n")
  (with type (assoc-ref l "type")
    (set! type (and (pair? type) (car type)))
    ;;(display* "    type= " type "\n")
    (with cv (lambda (f)
               (cons (car f)
                     (map (cut db-decode-entry type (car f) <>) (cdr f))))
      (if type (map cv l) l))))
