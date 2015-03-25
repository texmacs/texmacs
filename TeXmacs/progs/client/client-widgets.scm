
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
  (with-remote-search-user users server (list)
    (set! current-permissions (make-ahash-table))
    (with-remote-get-entry entry server id
      (for (attr attrs)
        (with vals (or (assoc-ref entry attr) (list))
          (ahash-set! current-permissions attr vals)
          (set! users (list-union users vals))))
      (with-remote-get-user-pseudo pseudos server users
        (with-remote-get-user-name names server users
          (for-each (lambda (user pseudo name)
                      (when (and (string? pseudo) (string? name))
                        (with full (string-append pseudo " (" name ")")
                          (ahash-set! current-user-decode user full)
                          (ahash-set! current-user-encode full user))))
                    users pseudos names)
          (set! users (list-difference users (list "all")))
          (set! current-user-ids users)
          (dialogue-window (entry-permissions-editor server id attrs)
                           noop
                           "Change permissions"))))))

(tm-define (open-file-permissions-editor server u)
  (:interactive #t)
  (with name (resource-cache-get (url->string u) "name")
    (with-remote-search ids server (list (cons "name" name))
      (when (pair? ids)
        (with attrs (list "readable" "writable" "owner")
          (open-entry-permissions-editor server (car ids) attrs))))))
