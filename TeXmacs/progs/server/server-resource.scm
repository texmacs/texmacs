
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-resource.scm
;; DESCRIPTION : Resources on TeXmacs servers
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-resource)
  (:use (server server-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution of SQL commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sdb (url-concretize "$TEXMACS_HOME_PATH/server/server.db"))

(tm-define (server-init-database)
  (when (not (url-exists? sdb))
    (sql-exec sdb "CREATE TABLE props (rid text, attr text, val text)")))

(tm-define (server-sql . l)
  (server-init-database)
  ;;(display* (apply string-append l) "\n")
  (sql-exec sdb (apply string-append l)))

(tm-define (server-sql* . l)
  (with r (apply server-sql l)
    (with f (lambda (x) (and (pair? x) (car x)))
      (map f (if (null? r) r (cdr r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic ressources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (resource-insert rid attr val)
  (server-sql "INSERT INTO props VALUES ('" rid "', '" attr "', '" val "')"))

(tm-define (resource-remove rid attr val)
  (server-sql "DELETE FROM props WHERE rid='" rid
              "' AND attr='" attr "', AND val='" val "'"))

(tm-define (resource-set rid attr vals)
  (resource-reset rid attr)
  (for-each (cut resource-insert rid attr <>) vals))

(tm-define (resource-reset rid attr)
  (server-sql "DELETE FROM props WHERE rid='" rid "' AND attr='" attr "'"))

(tm-define (resource-attributes rid)
  (server-sql* "SELECT DISTINCT attr FROM props WHERE rid='" rid "'"))

(tm-define (resource-get rid attr)
  (server-sql* "SELECT DISTINCT val FROM props WHERE rid='" rid
               "' AND attr='" attr "'"))

(tm-define (resource-get-first rid attr default)
  (with l (resource-get rid attr)
    (if (null? l) default (car l))))

(tm-define (resource-create name type uid)
  (with rid (create-unique-id)
    (if (nnull? (resource-get rid "type"))
        (resource-create name type uid)
        (begin
          (resource-insert rid "name" name)
          (resource-insert rid "type" type)
          (resource-insert rid "owner" uid)
          rid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching ressources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resource-search-join l i)
  (with s (string-append "props AS p" (number->string i))
    (if (null? (cdr l)) s
        (string-append s " JOIN " (resource-search-join (cdr l) (+ i 1))))))

(define (resource-search-on l i)
  (with (attr val) (car l)
    (let* ((pi (string-append "p" (number->string i)))
           (srid (string-append pi ".rid=p1.rid"))
           (sattr (string-append pi ".attr='" attr "'"))
           (sval (string-append pi ".val='" val "'"))
           (spair (string-append sattr " AND " sval))
           (q (if (= i 1) spair (string-append srid " AND " spair))))
      (if (null? (cdr l)) q
          (string-append q " AND " (resource-search-on (cdr l) (+ i 1)))))))

(tm-define (resource-search l)
  (if (null? l)
      (server-sql* "SELECT DISTINCT rid FROM props")
      (let* ((join (resource-search-join l 1))
             (on (resource-search-on l 1))
             (sep (if (null? (cdr l)) " WHERE " " ON ")))
        (server-sql* "SELECT DISTINCT p1.rid FROM " join sep on))))

(tm-define (resource-search-name name)
  (resource-search (list (list "name" name))))

(tm-define (resource-search-owner owner)
  (resource-search (list (list "owner" owner))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access rights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (resource-set-user-info uid id fullname email)
  (resource-set uid "id" (list id))
  (resource-set uid "full-name" (list fullname))
  (resource-set uid "type" (list "user"))
  (resource-set uid "owner" (list uid))
  (resource-set uid "email" (list email))
  (with home (string-append "~" id)
    (when (null? (resource-search (list (list "name" home)
                                        (list "type" "dir"))))
      (resource-create home "dir" uid))))

(define (resource-allow-many? rids rdone uid udone attr)
  (and (nnull? rids)
       (or (resource-allow-one? (car rids) rdone uid udone attr)
           (resource-allow-many? (cdr rids) rdone uid udone attr))))

(define (resource-allow-groups? rid rdone uids udone attr)
  (and (nnull? uids)
       (or (resource-allow-one? rid rdone (car uids) udone attr)
           (resource-allow-groups? rid rdone (cdr uids) udone attr))))

(define (resource-allow-one? rid rdone uid udone attr)
  ;;(display* "Allow one " rid ", " uid ", " attr "\n")
  (and (not (in? rid rdone))
       (not (in? uid udone))
       (or (== rid uid)
           (== rid "all")
           (with rids (append (resource-get rid attr)
                              (resource-get rid "owner"))
             (set! rids (list-remove-duplicates rids))
             (set! rids (list-difference rids (cons rid rdone)))
             (resource-allow-many? rids (cons rid rdone) uid udone attr))
           (with grs (resource-get uid "member")
             (resource-allow-groups? rid rdone grs (cons uid udone) attr)))))

(tm-define (resource-allow? rid uid attr)
  ;;(display* "Allow " rid ", " uid ", " attr "\n")
  (resource-allow-one? rid (list) uid (list) attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for changing properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-reserved-attributes)
  (list "type" "location" "dir" "date" "id"))

(tm-define (resource-get-all rid)
  (with t (make-ahash-table)
    (for (attr (resource-attributes rid))
      (ahash-set! t attr (resource-get rid attr)))
    (ahash-table->list t)))

(tm-define (resource-set-all rid props)
  (let* ((old (resource-attributes rid))
         (new (map car props))
         (del (list-difference old (append new (server-reserved-attributes)))))
    (for (attr del)
      (resource-reset rid attr)))
  (for (prop props)
    (when (and (pair? prop) (list? (cdr prop))
               (nin? (car prop) (server-reserved-attributes))
               (or (!= (car prop) "owner") (nnull? (cdr prop))))
      (resource-set rid (car prop) (cdr prop)))))

(define (user-decode rid)
  (if (== rid "all") rid
      (resource-get-first rid "id" #f)))

(define (user-encode user)
  (if (== user "all") user
      (with l (resource-search (list (list "type" "user") (list "id" user)))
        (and (pair? l) (car l)))))

(define (prop-decode x)
  (with (attr . vals) x
    (if (nin? attr '("owner" "readable" "writable")) x
        (cons attr (list-difference (map user-decode vals) (list #f))))))

(define (prop-encode x)
  (with (attr . vals) x
    (if (nin? attr '("owner" "readable" "writable")) x
        (cons attr (list-difference (map user-encode vals) (list #f))))))

(tm-define (resource-properties-decode l)
  ;;(display* "decode " l " -> " (map prop-decode l) "\n")
  (map prop-decode l))

(tm-define (resource-properties-encode l)
  ;;(display* "encode " l " -> " (map prop-encode l) "\n")
  (map prop-encode l))

(define (first-leq? p1 p2)
  (string<=? (car p1) (car p2)))

(tm-define (resource-get-all-decoded rid)
  (with raw-props (sort (resource-get-all rid) first-leq?)
    (resource-properties-decode raw-props)))

(tm-define (resource-set-all-encoded rid props)
  (with raw-props (resource-properties-encode props)
    (resource-set-all rid raw-props)))
