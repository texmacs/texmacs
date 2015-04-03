
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
;; Allow query preferences to be set on a pro-file basis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-get-query-preference file attr . opt-default)
  (when (url? file) (set! file (url->string file)))
  (let* ((key (string-append attr "," file))
         (r (get-preference key))
         (default (and (pair? opt-default) (car opt-default))))
    (if (!= r "default") r default)))

(tm-define (db-set-query-preference file attr val)
  (when (url? file) (set! file (url->string file)))
  (let* ((key (string-append attr "," file)))
    (set-preference key val)))

(tm-define (db-get-current-query file)
  (list (cons "search" (db-get-query-preference file "search" ""))
        (cons "order" (db-get-query-preference file "order" "name"))
        (cons "direction" (db-get-query-preference file "direction" "ascend"))
        (cons "limit" (db-get-query-preference file "limit" "10"))
        (cons "present" (db-get-query-preference file "present" "detailed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving the database queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              (cons "file" t)
              (cons "tmfs" (string-append "tmfs://db/" name))))))

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

(define (entry-present e p)
  (cond ((not (tm-func? e 'db-entry)) e)
        ((== p "folded") `(db-folded-entry ,@(tm-children e)))
        ((== p "pretty") `(db-pretty-entry ,@(tm-children e)))
        (else e)))

(define (get-db-fields a)
  (let* ((file (or (assoc-ref a "tmfs") "unknown"))
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
                 (r (map db-load-entry ids))
                 (present (or (assoc-ref a "present") "detailed")))
            (map (cut entry-present <> present) r)))))))

(tmfs-load-handler (db name)
  (let* ((a (name->query name))
         (kind (or (assoc-ref a "kind") "unknown"))
         (l (get-db-fields a))
         (l* (if (null? l) (list "") l)))
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple ,(db-get-style kind)))
       (body (document ,@l*)))))
