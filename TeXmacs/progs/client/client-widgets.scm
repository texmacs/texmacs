
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

(define (get-permissions u attr)
  (with props (resource-cache-get-all u)
    (list-union (or (assoc-ref props attr) (list))
                (or (assoc-ref props "owner") (list)))))

(define (set-permissions u attr vals)
  (with props (resource-cache-get-all u)
    (set! props (assoc-set! props attr vals))
    (client-set-file-properties u props)))

(tm-widget ((permissions-editor u) quit)
  (padded
    (with connections (sort (list-union (get-permissions u "readable")
                                        (get-permissions u "writable")
                                        (get-permissions u "owner"))
                            string<=?)
      (tabs
        (tab (text "Read")
          (padded
            (choices (set-permissions u "readable" answer)
                     connections
                     (get-permissions u "readable"))))
        (tab (text "Write")
          (padded
            (choices (set-permissions u "writable" answer)
                     connections
                     (get-permissions u "writable"))))
        (tab (text "Owner")
          (padded
            (choices (set-permissions u "owner" answer)
                     connections
                     (get-permissions u "owner"))))))))

(tm-define (open-permissions-editor)
  (:interactive #t)
  (with u (url->string (current-buffer))
    (dialogue-window (permissions-editor u) noop "Change permissions")))
