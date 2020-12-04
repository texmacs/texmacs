
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : part-shared.scm
;; DESCRIPTION : shared documents or parts of documents (active linking)
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (part part-shared)
  (:use (utils plugins plugin-eval)))

;; TODO: better undo in case of several mirrors in one document
;; TODO: nested mirror don't work appropriately (due to unique identifier
;; separation, which should not be enforced in this case).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mirror-context? t)
  (and (or (tree-func? t 'mirror 3)
           (tree-func? t 'shared 3)
           (tree-func? t 'show-comment 6)
           (tree-func? t 'hide-comment 6)
           (tree-func? t 'mirror-comment 6))
       (tree-atomic? (tree-ref t 0))
       (tree-atomic? (tree-ref t 1))))

(define (mirror-body? t)
  (and (tree->path t)
       (mirror-context? (tree-up t))
       (== (tree-index t) (- (tree-arity (tree-up t)) 1))))

(define (mirror-unique-id t)
  (or (and (mirror-context? t) (tree->string (tree-ref t 0)))
      (and (mirror-body? t) (mirror-unique-id (tree-up t)))))

(define (mirror-id t)
  (or (and (mirror-context? t) (tree->string (tree-ref t 1)))
      (and (mirror-body? t) (mirror-id (tree-up t)))))

(define (mirror-list t)
  (cond ((mirror-body? t)
         (with other? (lambda (u) (!= (tree->path t) (tree->path u)))
           (list-filter (id->trees (mirror-id t)) other?)))
        ((mirror-context? t)
         (mirror-list (tree-ref t (- (tree-arity t) 1))))
        (else (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new mirrors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-mirror name)
  (:argument name "Channel")
  (let* ((l (id->trees name))
         (id (create-unique-id)))
    (if (and (nnull? l) (mirror-body? (car l)))
        (let* ((doc (tm->stree (car l)))
               (p (path-start doc '())))
          (insert-go-to `(mirror ,id ,name ,doc) (cons 2 p)))
        (insert-go-to `(mirror ,id ,name "") (list 2 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization and expensive synchronization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mirror-author (new-author))
(define mirror-initialized (make-ahash-table))
(define mirror-black-list (make-ahash-table))

(define (mirror-white-listed? t)
  (not (ahash-ref mirror-black-list (mirror-unique-id t))))

(define (mirror-in-sync? t wrt)
  (and (mirror-white-listed? t) (== t wrt)))

(define (mirror-up-to-date? t)
  (list-and (map (cut mirror-in-sync? <> t) (mirror-list t))))

(define (mirror-invalidate t)
  ;;(display* "Invalidate " (mirror-id t) ", " (mirror-unique-id t)
  ;;", " (tm->stree t) "\n")
  (when (and (tree->path t) (mirror-body? t) (mirror-unique-id t))
    (when (== (ahash-size mirror-black-list))
      (delayed
        (:idle 1)
        (mirror-separate)
        (delayed
          (:idle 1)
          (mirror-synchronize))))
    (ahash-set! mirror-black-list (mirror-unique-id t) #t)))

(define (mirror-separate)
  (with-author mirror-author
    (with-global mirror-idle? #f
      (for (uid (ahash-set->list mirror-black-list))
        (with l (id->trees uid)
          (when (>= (length l) 2)
            ;;(display* "Separating " uid "\n")
            (ahash-remove! mirror-black-list uid)
            (for (t l)
              (when (mirror-body? t)
                (with nid (create-unique-id)
                  (tree-set (tree-ref t :up 0) nid)
                  (ahash-set! mirror-black-list nid #t))))))))))

(define (mirror-synchronize)
  (with-author mirror-author
    (with-global mirror-idle? #f
      (with failed (make-ahash-table)
        (for (uid (ahash-set->list mirror-black-list))
          (for (t (id->trees uid))
            (when (not (mirror-up-to-date? t))
              (with l (list-filter (mirror-list t) mirror-white-listed?)
                (when (null? l)
                  (ahash-set! failed (mirror-id t) #t))
                (when (nnull? l)
                  (tree-set! t (tree-copy (car l))))))))
        (for (id (ahash-set->list failed))
          ;;(display* "Forcefully synchronize " id "\n")
          (with l (id->trees id)
            (when (>= (length l) 2)
              (for (t (cdr l))
                (tree-set! t (tree-copy (car l))))))))
      (set! mirror-black-list (make-ahash-table)))))

(tm-define (mirror-initialize t)
  (:secure #t)
  (when (mirror-body? t)
    (with key (cons (mirror-id t) (mirror-unique-id t))
      (when (not (ahash-ref mirror-initialized key))
        ;;(display* "Initializing " key "\n")
        (when (not (mirror-up-to-date? t))
          (mirror-invalidate t))
        (ahash-set! mirror-initialized key #t))
      (when (>= (length (id->trees (mirror-unique-id t))) 2)
        (mirror-invalidate t))))
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mirror-idle? #t)
(define mirror-pending (make-ahash-table))

(define (mirror-treat-pending)
  (with-author mirror-author
    (with-global mirror-idle? #f
      (for (key-im (ahash-table->list mirror-pending))
        (when (not (ahash-ref mirror-black-list (car key-im)))
          (let* ((l (id->trees (car key-im)))
                 (ok? (== (length l) 1))
                 (t (and (nnull? l) (car l))))
            (for (mod (reverse (cdr key-im)))
              (when ok?
                (if (modification-applicable? t mod)
                    (begin
                      ;;(display* "Applying " (modification->scheme mod)
                      ;;" to " (tm->stree t) "\n")
                      (modification-apply! t mod))
                    (set! ok? #f))))
            (when (and t (not ok?))
              (mirror-invalidate t)))))
      (set! mirror-pending (make-ahash-table)))))

(define (mirror-list-notify l mod)
  (for (m l)
    (with uid (mirror-unique-id m)
      (when uid
        (when (== (ahash-size mirror-pending) 0)
          (delayed (:idle 1) (mirror-treat-pending)))
        (with old (or (ahash-ref mirror-pending uid) (list))
          (ahash-set! mirror-pending uid
                      (cons (modification-copy mod) old)))))))

(tm-define (mirror-notify event t mod)
  (:secure #t)
  (when (and mirror-idle?
             ;;(not (busy-versioning?))
             (== event 'announce)
             (!= (modification-type mod) 'set-cursor)
             (mirror-body? t))
    ;;(display* event ", " (tree->path t) ", "
    ;;(modification->scheme mod) "\n")
    (and-with p (tree-up t)
      (when (tree-func? p 'shared)
        (buffer-initialize-shared (mirror-id p))
        (buffer-notify-shared (mirror-id p) mod)))
    (with l (mirror-list t)
      (mirror-list-notify l mod))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sharing entire buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define buffer-shared-list (make-ahash-table))
(define buffer-pending (make-ahash-table))
(define buffer-black-list (make-ahash-table))

(tm-define (buffer-initialize id t buf)
  (ahash-set! buffer-shared-list id #t))

(tm-define (buffer-notify event t mod)
  (when (and mirror-idle?
             ;;(not (busy-versioning?))
             (== event 'announce)
             (!= (modification-type mod) 'set-cursor))
    (with ids (list-filter (tree->ids t)
                           (cut ahash-ref buffer-shared-list <>))
      (when (== (length ids) 1)
        (let* ((other? (lambda (u) (!= (tree->path t) (tree->path u))))
               (l (list-filter (id->trees (car ids)) other?)))
          (mirror-list-notify l mod))))))

(tm-define (buffer-initialize-shared id)
  (with buf id
    ;;(when (not (buffer-exists? buf))
    ;;  (delayed
    ;;    (:idle 1)
    ;;    (load-buffer buf :background)))
    (if (not (ahash-ref buffer-shared-list id))
        (when (buffer-exists? buf)
          (buffer-attach-notifier buf)
          (ahash-set! buffer-shared-list id #t)
          (ahash-set! buffer-black-list id #t))
        (when (not (buffer-exists? buf))
          (ahash-remove! buffer-shared-list id)))))

(define (buffer-restore buf)
  (with-author mirror-author
    (with-global mirror-idle? #f
      (let* ((t (buffer-get-body buf))
             (other? (lambda (u) (!= (tree->path t) (tree->path u))))
             (l (list-filter (id->trees buf) other?)))
        (when (nnull? l)
          (tree-set! t (tree-copy (car l))))
        (ahash-remove! buffer-black-list buf)))))

(define (buffer-treat-pending)
  (with-author mirror-author
    (with-global mirror-idle? #f
      (for (key-im (ahash-table->list buffer-pending))
        (let* ((id (car key-im))
               (ok? (not (ahash-ref buffer-black-list id)))
               (t (buffer-get-body id)))
          (for (mod (reverse (cdr key-im)))
            (when ok?
              (if (modification-applicable? t mod)
                  (begin
                    ;;(display* "Applying " (modification->scheme mod)
                    ;;" to " (tm->stree t) "\n")
                    (modification-apply! t mod))
                  (set! ok? #f))))
          (when (and t (not ok?))
            (buffer-restore id))))
      (set! buffer-pending (make-ahash-table)))))

(tm-define (buffer-notify-shared id mod)
  (when (ahash-ref buffer-shared-list id)
    (when (== (ahash-size buffer-pending) 0)
      (delayed (:idle 1) (buffer-treat-pending)))
    (with old (or (ahash-ref buffer-pending id) (list))
      (ahash-set! buffer-pending id
                  (cons (modification-copy mod) old)))))
