
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
  (:use (database db-users)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating a new version of an entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-same-entries? e1 e2)
  (let* ((ok? (lambda (f) (and (pair? f) (nin? (car f) (db-meta-attributes)))))
         (l1 (list-filter e1 ok?))
         (l2 (list-filter e2 ok?)))
    (list-permutation? l1 l2)))

(tm-define (db-update-entry id new-l)
  (with old-l (db-get-entry id)
    (if (db-same-entries? new-l old-l) id
        (let* ((new-h (cons id (or (assoc-ref new-l "newer") (list))))
               (old-h (or (assoc-ref old-l "newer") (list)))
               (app-h (list-union new-h old-h)))
          (set! new-l (assoc-set! new-l "newer" app-h))
          (with new-id (db-create-entry new-l)
            (with-extra-fields (list)
              (db-set-entry id (list)))
            new-id)))))
