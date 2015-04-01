
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-tmfs.scm
;; DESCRIPTION : databases as documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-tmfs)
  (:use (database db-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizing searches for speed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-optimized-search* q)
  (receive (l1 l2) (list-partition q (cut func? <> :match))
    (with faster? (lambda (q1 q2)
                    (<= (index-number-matches (cadr q1))
                        (index-number-matches (cadr q2))))
      (db-search (append (sort l1 faster?) l2)))))

(tm-define (db-optimized-search q)
  (if (not (func? (car q) :prefix))
      (db-optimized-search* q)
      (let* ((l (index-get-completions (cadar q)))
             (t (make-ahash-table)))
        (for (key l)
          (when (< (ahash-size t) db-limit)
            (with r (db-optimized-search* (cons (list :match key) (cdr q)))
              (for (id r)
                (ahash-set! t id #t)))))
        (with r (ahash-set->list t)
          (if (<= (length r) db-limit) r (sublist r 0 db-limit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving the database queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-query-current (make-ahash-table))

(define (db-get-file file)
  (when (and (nstring? file) (url? file))
    (set! file (url->string file)))
  (if (string-starts? file "tmfs://db/")
      (with a (name->query (string-drop file (string-length "tmfs://db/")))
        (or (assoc-ref a "file") "unknown"))
      file))

(tm-define (db-get-current-query file)
  (set! file (db-get-file file))
  (or (ahash-ref db-query-current file) (list)))

(tm-define (db-set-current-query file q)
  (set! file (db-get-file file))
  (ahash-set! db-query-current file q))

(define (name->query name)
  (let* ((h (tmfs-car name))
         (t (tmfs-cdr name))
         (i (string-search-forwards "=" 0 h)))
    (if (>= i 0)
        (let* ((l (name->query t))
               (var (substring h 0 i))
               (val (substring h (+ i 1) (string-length h))))
          (assoc-set! l var val))
        (list (cons "kind" h)
              (cons "file" t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-style kind)
  (string-append "database-" kind))

(tmfs-title-handler (db name doc)
  (let* ((a (name->query name))
         (kind (or (assoc-ref a "kind") "unknown"))
         (file (or (assoc-ref a "file") "unknown"))
         (type (cond ((== kind "bib") "bibliographic database")
                     (else (string-append kind " database")))))
    (if (== file "global")
        (string-append "My " type)
        (string-append (upcase-first type) " - " file))))

(define (get-db-fields a)
  (let* ((file (or (assoc-ref a "file") "unknown"))
         (a* (db-get-current-query file)))
    (set! a (assoc-add a a*))
    (with-database (user-database)
      (with-indexing #t
        (with-limit (with limit (assoc-ref a "limit")
                      (or (and limit (string->number limit)) 10))
          (let* ((search (or (assoc-ref a "search") ""))
                 (ss (list-filter (string-tokenize-comma search)
                                  (lambda (s) (>= (string-length s) 2))))
                 (sq (map (lambda (s) (list :match s)) ss))
                 (asc? (!= (assoc-ref a "direction") "descend"))
                 (order (or (assoc-ref a "order") "name"))
                 (os* (string-tokenize-comma order))
                 (os (list-filter os* (cut != <> "")))
                 (oq (map (lambda (s) (list :order s asc?)) os))
                 (kind (or (assoc-ref a "kind") "unknown"))
                 (types (or (smart-ref db-kind-table kind) (list)))
                 (q (append sq (list (cons "type" types)) oq))
                 (ids (db-optimized-search q))
                 (r (map db-load-entry ids)))
            r))))))

(tmfs-load-handler (db name)
  (let* ((a (name->query name))
         (kind (or (assoc-ref a "kind") "unknown"))
         (l (get-db-fields a))
         (l* (if (null? l) (list "") l)))
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple ,(db-get-style kind)))
       (body (document ,@l*)))))
