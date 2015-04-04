
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
;; Pretty typesetting with cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-pretty-cache (make-ahash-table))

(define (db-pretty-cached t kind fm)
  (when (not (ahash-ref db-pretty-cache (list kind fm)))
    (ahash-set! db-pretty-cache (list kind fm) (make-ahash-table)))
  (with cache (ahash-ref db-pretty-cache (list kind fm))
    (with st (tm->stree t)
      (or (ahash-ref cache st)
          (with r (car (db-pretty (list st) kind fm))
            (when (tm-func? r 'db-result 2)
              ;;(display* (tm-ref t 2) ", " (tree->path (tm-ref t 2)) "\n")
              (set! r `(db-pretty ,(tm-ref t 2) ,(tm-ref r 1))))
            ;;(display* "r= " (tm->stree r) "\n")
            (ahash-set! cache st r)
            r)))))

(tm-define (ext-db-pretty-entry kind rid type name meta body)
  (:secure #t)
  (with t `(db-entry ,rid ,type ,name ,meta ,body)
    (db-pretty-cached t (tm->string kind) :pretty)))

(tm-define (db-pretty-notify t)
  (:secure #t)
  (when (and (tree-up t) (tree-is? (tree-up t) 'db-pretty-entry))
    (tree-go-to t :start)
    (tree-assign-node (tree-up t) 'db-entry)))
