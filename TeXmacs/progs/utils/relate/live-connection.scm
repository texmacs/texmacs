
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : live-connection.scm
;; DESCRIPTION : Remote connection management for live shared documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils relate live-connection)
  (:use (utils relate live-document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful utility: conversions between patches and lists of modifications
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
    (ahash-set! live-connections lid (make-ahash-table)))
  (with t (ahash-ref live-connections lid)
    (ahash-set! t remote state)))

(tm-define (live-get-remote-state lid remote)
  (when (not (ahash-ref live-connections lid))
    (ahash-set! live-connections lid (make-ahash-table)))
  (with t (ahash-ref live-connections lid)
    (ahash-ref t remote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage states in use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (live-connection-states lid)
  (when (not (ahash-ref live-connections lid))
    (ahash-set! live-connections lid (make-ahash-table)))
  (with t (ahash-ref live-connections lid)
    (map cdr (ahash-table->list t))))

(tm-define (live-states-in-use lid)
  (append (former lid) (live-connection-states lid)))
