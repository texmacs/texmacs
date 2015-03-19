
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
  (set! p (patch-copy p))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get information about or up to a given state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-get-document lid state)
  (let* ((doc (live-current-document lid))
         (p (live-get-patch lid state)))
    (and doc p (patch-apply doc p))))

(define (live-state-list state states)
  (if (== state (car states)) (list)
      (cons (car states) (live-state-list state (cdr states)))))

(tm-define (live-get-state-list lid state)
  (with states (ahash-ref live-states lid)
    (live-state-list state states)))

(define (live-patch-list state states changes)
  (if (== state (car states)) (list)
      (cons (car changes)
            (live-patch-list state (cdr states) (cdr changes)))))

(tm-define (live-get-patch-list lid state)
  (let* ((states (ahash-ref live-states lid))
         (changes (ahash-ref live-changes lid)))
    (and states changes (in? state states)
         (live-patch-list state states changes))))

(tm-define (live-get-patch lid state)
  (and-with l (live-get-patch-list lid state)
    (patch-compound l)))

(tm-define (live-get-inverse-patch lid state)
  (and-with doc (live-current-document lid)
    (and-with p (live-get-patch lid state)
      (patch-invert p doc))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing simultaneous modifications in same live document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (live-find-latest-compatible state states changes p t)
  (if (== state (car states))
      (list state p)
      (let* ((u (patch-apply t (car changes)))
             (states* (cdr states))
             (changes* (cdr changes))
             (r (live-find-latest-compatible state states* changes* p u)))
        (with (best p*) r
          (with ok? (and (nnull? (cdr states))
                         (== best (cadr states))
                         (patch-can-pull? p* (car changes))
                         (let* ((q (car changes))
                                (p** (patch-pull p* q))
                                (q* (patch-co-pull p* q))
                                (u* (patch-apply u p*))
				(t* (patch-apply t p**)))
                           (== (patch-apply t* q*) u*)))
	    (when (not ok?)
	      (display* "-- Conflicting patches for " (tm->stree u) "\n")
	      (display* "-- Patch 1: " (patch->scheme p*) "\n")
	      (with iq (patch-invert (car changes) t)
		(display* "-- Patch 2: " (patch->scheme iq) "\n"))
	      (display* "-- best  = " best "\n")
	      (display* "-- states= " states "\n")
	      (display* "-- can-pull? "
			(patch-can-pull? p* (car changes)) "\n"))
	    ;;(if ok? (list state (patch-pull p* (car changes))) r))))))
            (if ok? (list (car states) (patch-pull p* (car changes))) r))))))

(tm-define (live-latest-compatible lid state p)
  (let* ((doc (live-current-document lid))
         (states (ahash-ref live-states lid))
         (changes (ahash-ref live-changes lid)))
    (car (live-find-latest-compatible state states changes p doc))))

(tm-define (live-retract lid state)
  (and (in? state (ahash-ref live-states lid))
       (while (not (== state (live-current-state lid)))
         (let* ((doc (live-current-document lid))
                (states (ahash-ref live-states lid))
                (changes (ahash-ref live-changes lid)))
	   (display* "Retract " (patch->scheme (car changes)) "\n")
           (ahash-set! live-documents lid (patch-apply doc (car changes)))
           (ahash-set! live-states lid (cdr states))
           (ahash-set! live-changes lid (cdr changes))))))

(tm-define (live-rewrite-history lid until new-states new-changes)
  (set! new-changes (map patch-copy new-changes))
  (let* ((old-states (live-get-state-list lid until))
         (old-changes (live-get-patch-list lid until))
         (old-p (patch-compound old-changes))
         (new-p (patch-compound new-changes)))
    (when (or (!= new-states old-states)
              (!= (map patch->scheme new-changes)
                  (map patch->scheme old-changes)))
      (display* ">> until= " until "\n")
      (display* ">> old-states= " old-states "\n")
      (display* ">> new-states= " new-states "\n")
      (display* ">> old-changes= " (map patch->scheme old-changes) "\n")
      (display* ">> new-changes= " (map patch->scheme new-changes) "\n")
      (when (not (and-with doc (live-current-document lid)
                   (patch-strong-equivalent? old-p new-p doc)))
        (texmacs-error "live-rewrite-history" "non equivalent history"))
      (let* ((all-states (ahash-ref live-states lid))
             (all-changes (ahash-ref live-changes lid)))
        ;;(display* ">> all-states= " old-states "\n")
        ;;(display* ">> all-changes= " (map patch->scheme all-changes) "\n")
        (while (!= (car all-states) until)
          (set! all-states (cdr all-states))
          (set! all-changes (cdr all-changes)))
        (ahash-set! live-states lid (append new-states all-states))
        (ahash-set! live-changes lid (append new-changes all-changes))))))
