
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-markup.scm
;; DESCRIPTION : complementary scheme-based database markup
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-markup)
  (:use (database db-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty printing with cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-pretty-cache (make-ahash-table))

(define (db-pretty-cached t kind fm)
  (when (not (ahash-ref db-pretty-cache (list kind fm)))
    (ahash-set! db-pretty-cache (list kind fm) (make-ahash-table)))
  (with cache (ahash-ref db-pretty-cache (list kind fm))
    (with st (tm->stree t)
      (or (ahash-ref cache st)
          (with r (db-pretty (list st) kind fm)
            (ahash-set! cache st (car r))
            (car r))))))

(tm-define (ext-db-pretty-entry rid type name meta body)
  (:secure #t)
  (with t `(db-entry ,rid ,type ,name ,meta ,body)
    (db-pretty-cached t "bib" :compact)))
