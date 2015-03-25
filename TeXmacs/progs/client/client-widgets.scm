
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-widgets.scm
;; DESCRIPTION : widgets for remote clients
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-widgets)
  (:use (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-user-ids (list))
(define current-user-encode (make-ahash-table))
(define current-user-decode (make-ahash-table))
(define current-permissions (make-ahash-table))

(define (set-permissions* server id attr vals*)
  (with vals (map (cut ahash-ref current-user-encode <>) vals*)
    (ahash-set! current-permissions attr vals)
    (remote-set-field server id attr vals)))

(define (get-permissions* server id attr)
  (with vals (list-union (ahash-ref current-permissions "owner")
                         (ahash-ref current-permissions attr))
    (with vals* (map (cut ahash-ref current-user-decode <>) vals)
      (sort vals* string<=?))))

(tm-widget ((entry-permissions-editor server id attrs) quit)
  (padded
    (tabs
      (loop (attr attrs)
        (tab (text (upcase-first attr))
          (padded
            (choices (set-permissions* server id attr answer)
                     (map (cut ahash-ref current-user-decode <>)
                          current-user-ids)
                     (get-permissions* server id attr))))))))

(tm-define (open-entry-permissions-editor server id attrs)
  (:interactive #t)
  (with-remote-search-user r server (list)
    (set! current-user-ids r)
    (set! current-permissions (make-ahash-table))
    (open-entry-permissions-editor* server id attrs attrs current-user-ids)))

(define (open-entry-permissions-editor* server id attrs la lu)
  (cond ((nnull? la)
         (with-remote-get-field r server id (car la)
           (ahash-set! current-permissions (car la) r)
           (set! current-user-ids (list-union current-user-ids r))
           (open-entry-permissions-editor* server id attrs (cdr la) lu)))
        ((nnull? lu)
         (with-remote-get-user-pseudo pseudo server (car lu)
           (with-remote-get-user-name name server (car lu)
             (with full (string-append pseudo " (" name ")")
               (ahash-set! current-user-decode (car lu) full)
               (ahash-set! current-user-encode full (car lu)))
             (open-entry-permissions-editor* server id attrs la (cdr lu)))))
        (else
          (set! current-user-ids
                (list-difference current-user-ids (list "all")))
          (dialogue-window (entry-permissions-editor server id attrs)
                           noop
                           "Change permissions"))))

(tm-define (open-file-permissions-editor server u)
  (:interactive #t)
  (with name (resource-cache-get (url->string u) "name")
    (with-remote-search ids server (list (cons "name" name))
      (when (pair? ids)
        (with attrs (list "readable" "writable" "owner")
          (open-entry-permissions-editor server (car ids) attrs))))))
