
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : live.scm
;; DESCRIPTION : Live shared documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils relate live-document))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage live documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-documents (make-ahash-table))
(define live-states (make-ahash-table))
(define live-changes (make-ahash-table))

(tm-define (live-create lid doc . opt-state)
  (with initial (if (null? opt-state) (create-unique-id) (car opt-state))
    (ahash-set! live-documents lid (tm->tree doc))
    (ahash-set! live-states lid (list initial))
    (ahash-set! live-changes lid (list))
    initial))

(tm-define (live-current-document lid)
  (ahash-ref live-documents lid))

(tm-define (live-current-state lid)
  (and (pair? (ahash-ref live-states lid))
       (car (ahash-ref live-states lid))))

(tm-define (live-get-history lid)
  (and (pair? (ahash-ref live-states lid))
       (ahash-ref live-states lid)))

(tm-define (live-apply-patch lid p . opt-state)
  (let* ((doc (live-current-document lid))
         (states (ahash-ref live-states lid))
         (changes (or (ahash-ref live-changes lid) (list)))
         (new-state (if (null? opt-state) (create-unique-id) (car opt-state))))
    (and doc states changes (patch-applicable? p doc)
         (with inv (patch-invert p doc)
           (ahash-set! live-documents lid (patch-apply doc p))
           (ahash-set! live-states lid (cons new-state states))
           (ahash-set! live-changes lid (cons inv changes))
           new-state))))

(define (live-patch-list state states changes)
  (if (== state (car states)) (list)
      (cons (car changes)
            (live-patch-list state (cdr states) (cdr changes)))))

(tm-define (live-get-patch lid state)
  (let* ((states (ahash-ref live-states lid))
         (changes (ahash-ref live-changes lid)))
    (and states changes (in? state states)
         (with l (live-patch-list state states changes)
           (patch-compound l)))))

(tm-define (live-get-inverse-patch lid state)
  (and-with doc (live-current-document lid)
    (and-with p (live-get-patch lid state)
      (patch-invert p doc))))

(tm-define (live-get-document lid state)
  (let* ((doc (live-current-document lid))
         (p (live-get-patch lid state)))
    (and doc p (patch-apply doc p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forgetting states which are no longer in use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-states-in-use lid)
  ;; for subsequent overloading
  (list))

(define (live-get-oldest l among)
  (with r (list-remove l (car among))
    (if (or (null? r) (null? (cdr among)))
        (car among)
        (live-get-oldest r (cdr among)))))

(tm-define (live-oldest-state lid)
  (let* ((used (live-states-in-use lid))
         (hist (live-get-history lid))
         (all (list-remove-duplicates hist)))
    (live-get-oldest used all)))

(tm-define (live-forget-prior lid state)
  ;; Forget all states which are strictly prior to @state.
  (when (in? state (ahash-ref live-states lid))
    (let* ((old-states (ahash-ref live-states lid))
           (old-changes (ahash-ref live-changes lid))
           (new-states (list))
           (new-changes (list)))
      (while (!= (car old-states) state)
        (set! new-states (cons (car old-states) new-states))
        (set! new-changes (cons (car old-changes) new-changes))
        (set! old-states (cdr old-states))
        (set! old-changes (cdr old-changes)))
      (set! new-states (cons state new-states))
      (ahash-set! live-states lid (reverse new-states))
      (ahash-set! live-changes lid (reverse new-changes)))))

(tm-define (live-forget-obsolete lid)
  (with oldest (live-oldest-state lid)
    (live-forget-prior lid oldest)))
