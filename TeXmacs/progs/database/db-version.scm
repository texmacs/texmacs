
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-version.scm
;; DESCRIPTION : version management for TeXmacs databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-version)
  (:use (database db-users) (database db-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warnings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-warning . l)
  (with msg (string-append (apply string-append l) "\n")
    (debug-message "database-warning" msg)
    ;;(display* msg)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating a new version of an entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-same-entries? e1 e2)
  (let* ((ok? (lambda (f) (and (pair? f) (nin? (car f) (db-meta-attributes)))))
         (l1 (list-filter e1 ok?))
         (l2 (list-filter e2 ok?)))
    (list-permutation? l1 l2)))

(tm-define (db-update-entry id new-l . opt-new-id)
  (with old-l (db-get-entry id)
    (if (db-same-entries? new-l old-l) id
        (let* ((new-h (cons id (or (assoc-ref new-l "newer") (list))))
               (old-h (or (assoc-ref old-l "newer") (list)))
               (app-h (list-remove-duplicates (append new-h old-h))))
          (set! new-l (assoc-set! new-l "newer" app-h))
          (when db-time-stamp?
            (set! new-l (assoc-remove! new-l "date")))
          (with new-id (if (and (nnull? opt-new-id)
                                (not (db-entry-exists? (car opt-new-id))))
                           (begin
                             (db-set-entry (car opt-new-id) new-l)
                             (car opt-new-id))
                           (db-create-entry new-l))
            (db-remove-entry id)
            new-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing entries.  In the case that the new entry clashes with
;; an existing one, reject the new entry if needed or consider
;; one of the two entries as an update of the other one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assoc-ref-first l x)
  (with r (assoc-ref l x)
    (and (pair? r) (car r))))

(define (db-find-entry l)
  (let* ((ids (db-search (list (cons "name" (assoc-ref l "name")))))
         (ok? (lambda (id) (db-same-entries? (db-get-entry id) l))))
    (list-filter ids ok?)))

(define (db-declare-supersedes nid ids)
  ;;(display* "  Supersedes " nid " >> " ids "\n")
  (with h (db-get-field nid "newer")
    (set! ids (list-difference ids h))
    (when (nnull? ids)
      (db-set-field nid "newer" (list-remove-duplicates (append ids h))))))

(define (db-declare-superseded id)
  (when (nnull? (db-get-entry id))
    ;;(display* "  Superseded " id "\n")
    (db-remove-entry id)))

(tm-define (db-import-entry id l)
  (let* ((name (assoc-ref-first l "name"))
         (contributor (or (assoc-ref-first l "contributor")
                          (assoc-ref-first db-extra-fields "contributor")))
         (sim (if (and name contributor)
                  (db-search (list (list "name" name)
                                   (list "contributor" contributor)))
                  (list)))
         (his (with h (assoc-ref l "newer") (or h (list)))))
    ;;(display* "Processing " name ", " contributor ", " id ", " sim "\n")
    (cond ((nnull? (db-get-entry id)) ;; new entries must have new identifiers
           (when (and db-duplicate-warning?
                      (not (db-same-entries? l (db-get-entry id))))
             (db-warning "Ignored entry " name " with existing identifier")))
          ((nnull? (db-find-entry l)) ;; find exact matches
           ;;(when (and db-duplicate-warning? (not exact?))
           ;;(db-warning "Existing entry " name))
           (for (xid (db-find-entry l))
             (db-declare-supersedes xid (list id))))
          ((nnull? (db-search (list (list "newer" id))))
           (when db-duplicate-warning?
             (db-warning "Newer version of " name " already available")))
          ((begin
             (for (oid his)
               (db-declare-superseded oid))
             #f)
           (noop))
          ((nnull? sim)
           (let* ((xid (car sim))
                  (xl (db-get-entry xid))
                  (date (assoc-ref-first l "date"))
                  (xdate (assoc-ref-first xl "date"))
                  (modus (or (assoc-ref-first l "modus")
                             (assoc-ref-first db-extra-fields "modus")))
                  (xmodus (assoc-ref-first xl "modus")))
             ;;(display* "Modus " xmodus ", " modus "\n")
             ;;(display* "Date  " xdate ", " date "\n")
             (cond ((and (== xmodus "manual") (!= modus "manual"))
                    (db-declare-supersedes xid (cons id his))
                    (when db-duplicate-warning?
                      (db-warning "Kept existing version of entry " name)))
                   ((and (== modus "manual") (!= xmodus "manual"))
                    (with newer (list-remove-duplicates (cons xid his))
                      (set! l (assoc-set! l "newer" newer)))
                    (db-set-entry id l)
                    (db-declare-superseded xid)
                    (when db-duplicate-warning?
                      (db-warning "Updated the entry " name)))
                   ((and (string? xdate)
                         (string->number xdate)
                         (or (not (string? date))
                             (not (string->number date))
                             (>= (string->number xdate)
                                 (string->number date))))
                    (db-declare-supersedes xid (cons id his))
                    (when db-duplicate-warning?
                      (db-warning "Kept existing version of entry " name)))
                   (else
                     (with newer (list-remove-duplicates (cons xid his))
                       (set! l (assoc-set! l "newer" newer)))
                     (db-set-entry id l)
                     (db-declare-superseded xid)
                     (when db-duplicate-warning?
                       (db-warning "Updated the entry " name))))))
          (else (db-set-entry id l)))))
