
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-live.scm
;; DESCRIPTION : Live shared documents (server side)
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-live)
  (:use (utils relate live-connection)
        (server server-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define live-waiting (make-ahash-table))

(define (live-applicable? lid client p old-state)
  (and (== old-state (live-current-state lid))
       (with doc (live-current-document lid)
         (patch-applicable? p doc))))

(define (live-apply lid client p old-state)
  (and (== old-state (live-current-state lid))
       (and-with new-state (live-apply-patch lid p)
         (live-set-remote-state lid client new-state)
         (live-broadcast lid)
         new-state)))

(define (live-update lid client state)
  (with key (list lid client)
    (when (not (ahash-ref live-waiting key))
      (ahash-set! live-waiting key #t)
      (let* ((p (live-get-patch lid state))
             (inv (patch-invert p (get-current-document lid)))
             (mods (patch->modlist inv))
             (new-state (live-current-state lid)))
        (server-remote-eval client `(live-modify ,lid ,mods ,state ,new-state)
          (lambda (ok?)
            (ahash-remove! live-waiting key)
            (when ok? (live-set-remote-state lid client new-state))
            (live-broadcast lid)))))))

(define (live-broadcast lid)
  (and-with t (ahash-ref live-connections lid)
    (for (key-im (ahash-table->list t))
      (with (client state) key-im
        (if (client-active? client)
            (when (!= state (live-current-state lid))
              (live-update lid client state))
            (live-hang-up lid client))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-service (live-open lid)
  ;; Connect client to the live channel lid
  ;;(display* "live-open " lid "\n")
  (when (not (live-current-document lid))
    (live-create lid '(document "")))
  (with (client msg-id) envelope
    (live-connect lid client)
    (let* ((doc (live-current-document lid))
           (state (live-get-remote-state lid client)))
      (server-return envelope (list state (tm->stree doc))))))

(tm-service (live-modify lid mods old-state)
  ;; States that the current state of the client is obtained
  ;; from 'old-state' by applying the list of modifications 'mods'
  ;;(display* "live-modify " lid ", " p ", " old-state "\n")
  (with p (modlist->patch mods) ;; TODO: to be implemented
    (with (client msg-id) envelope
      (cond ((not (ahash-ref live-connections (list lid client)))
             (server-error envelope "Error: non existent live document"))
            ((not (live-applicable? lid client p old-state))
             (server-return envelope #f))
            (else
              (with new-state (live-apply lid client p old-state)
                (server-return envelope new-state)))))))
