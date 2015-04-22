
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-sync.scm
;; DESCRIPTION : synchronizing client databases with the server
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-db-sync)
  (:use (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determining changes in a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-change-list uid kind t)
  (when (number? t) (set! t (number->string t)))
  (let* ((types (or (smart-ref db-kind-table kind) #t))
         ;; FIXME: db-kind-table may not be fully initialized
         (ids '()))
    (with-time :always
      (with-user #t
        (set! ids (db-search `(,@(if (== uid #t) (list)
                                     `(("owner" ,uid)))
                               ,@(if (== types #t) (list)
                                     `(("type" ,@types)))
                               (:modified ,t "10675199165"))))))
    (with-user #t
      (with get (lambda (id)
                  (list id
                        (with-time :always
                          (db-get-field-first id "name" #f))
                        (with-time :now
                          (db-get-entry id))))
        (map get ids)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building status list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-change-table l)
  (with t (make-ahash-table)
    (for (x l)
      (with (id name val) x
        (when (and name (or (not (ahash-ref t name)) (nnull? val)))
          (ahash-set! t name (list id val)))))
    t))

(define (db-equivalent? e1 e2)
  (for (x (cons* "owner" "readable" "writable" (db-meta-attributes)))
    (set! e1 (assoc-remove! e1 x))
    (set! e2 (assoc-remove! e2 x)))
  (with leq? (lambda (f1 f2) (string<=? (car f1) (car f2)))
    (== (sort e1 leq?) (sort e2 leq?))))

(define (db-change-status local-l remote-l)
  (display* "db-change-status ...\n")
  (let* ((local-t (db-change-table local-l))
         (remote-t (db-change-table remote-l))
         (status-t (make-ahash-table))
         (local-names (map car (ahash-table->list local-t)))
         (remote-names (map car (ahash-table->list remote-t)))
         (names (list-union local-names remote-names)))
    (display* "local-t= " local-t "\n")
    (display* "remote-t= " remote-t "\n")
    (display* "names= " names "\n")
    (for (name names)
      (let* ((local-pair (ahash-ref local-t name))
             (remote-pair (ahash-ref remote-t name))
             (local-id (and local-pair (car local-pair)))
             (remote-id (and remote-pair (car remote-pair)))
             (local-val (and local-pair (cadr local-pair)))
             (remote-val (and remote-pair (cadr remote-pair))))
        (cond ((and (null? local-val) (not remote-val))
               (ahash-set! status-t name (list "remote-delete")))
              ((and (null? remote-val) (not local-val))
               (ahash-set! status-t name (list "local-delete")))
              ((and local-val (not remote-val))
               (ahash-set! status-t name (list "upload" local-id local-val)))
              ((and remote-val (not local-val))
               (ahash-set! status-t name (list "download" remote-id remote-val)))
              ((and local-val remote-val (db-equivalent? local-val remote-val))
               (ahash-set! status-t name (list "download" remote-val)))
              (else
               (ahash-set! status-t name (list "conflict"
                                               local-id remote-id
                                               local-val remote-val))))))
    (with leq? (lambda (f1 f2) (string<=? (car f1) (car f2)))
      (sort (ahash-table->list status-t) leq?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-server)
  (with l (client-active-servers)
    (and (nnull? l) (car l))))

(tm-define (db-sync-test)
  (with local-l (with-database (user-database "bib")
                  (db-change-list #t #t "0"))
    (for (x local-l)
      (display* "Local: " x "\n"))
    (client-remote-eval (current-server) `(remote-db-changes "bib" "0")
      (lambda (remote-l)
        (for (x remote-l)
          (display* "Remote: " x "\n"))
        (with status-l (db-change-status local-l remote-l) 
          (for (x status-l)
            (display* "Status: " x "\n")))))))
