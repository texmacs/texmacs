
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

(tm-define (live-create lid doc)
  (ahash-set! live-documents lid (tm->tree doc))
  (ahash-set! live-states lid (list (create-unique-id)))
  (ahash-set! live-changes lid (list)))

(tm-define (live-current-document lid)
  (ahash-ref live-documents lid))

(tm-define (live-current-state lid)
  (and (pair? (ahash-ref live-states lid))
       (car (ahash-ref live-states lid))))

(tm-define (live-apply-patch lid p)
  (let* ((doc (live-current-document lid))
         (states (ahash-ref live-states lid))
         (changes (or (ahash-ref live-changes lid) (list)))
         (new-state (create-unique-id)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage live connections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-connections (make-ahash-table))

(tm-define (live-connect lid remote)
  (live-set-remote-state lid remote (live-current-state lid)))

(tm-define (live-hang-up lid remote)
  (when (not (ahash-ref live-connections lid))
    (ahash-set! live-connections (make-ahash-table)))
  (with t (ahash-ref live-connections lid)
    (ahash-remove! t remote)))

(tm-define (live-set-remote-state lid remote state)
  (when (not (ahash-ref live-connections lid))
    (ahash-set! live-connections (make-ahash-table)))
  (with t (ahash-ref live-connections lid)
    (ahash-set! t remote state)))

(tm-define (live-get-remote-state lid remote)
  (when (not (ahash-ref live-connections lid))
    (ahash-set! live-connections (make-ahash-table)))
  (with t (ahash-ref live-connections lid)
    (ahash-ref t remote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions between patches and lists of modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (patch->modlist p)
  (cond ((patch-pair? p) (list (modification->scheme (patch-direct p))))
        ((patch-compound? p) (append-map patch->modlist (patch-children p)))
        ((patch-branch? p) (patch->modlist (patch-ref p 0)))
        ((patch-birth? p) (list))
        ((patch-author? p) (patch->modlist (patch-ref p 0)))
        (else (list))))

(tm-define (modlist->patch mods t)
  (with r (list)
    (while (nnull? mods)
      (let* ((m (scheme->modification (car mods)))
             (inv (modification-invert m t))
             (p (patch-pair m inv)))        
        (set! r (cons p r))
        (set! t (modification-apply t m))
        (set! mods (cdr mods))))
    (patch-compound (reverse r))))
