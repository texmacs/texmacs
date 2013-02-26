
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-resource.scm
;; DESCRIPTION : resource management on the client side
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-resource)
  (:use (client client-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local cache with resource properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define resource-cache (make-ahash-table))

(tm-define (resource-cache-set-all u props)
  (ahash-set! resource-cache u props))

(tm-define (resource-cache-get-all u)
  (ahash-ref resource-cache u))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-url (url-none))
(define current-properties (list))
(define current-attribute "")

(define (default-attributes)
  (map car current-properties))

(define (default-values attr)
  (or (assoc-ref current-properties attr) '()))

(define (add-attribute attr)
  (when (and attr (not (assoc-ref current-properties attr)))
    (set! current-properties
          (assoc-set! current-properties attr '()))))

(define (add-value attr val)
  (when (and attr val)
    (with old-vals (or (assoc-ref current-properties attr) '())
      (when (nin? val old-vals)
        (with new-vals (rcons old-vals val)
          (set! current-properties
                (assoc-set! current-properties attr new-vals)))))))

(define (reserved-attributes)
  (list "name" "type" "location"))

(define (remove-attribute attr)
  (when (and attr (nin? attr (cons "owner" (reserved-attributes))))
    (set! current-properties
          (assoc-remove! current-properties attr))))

(define (remove-value attr val)
  (when (and attr val (nin? attr (reserved-attributes)))
    (with old-vals (or (assoc-ref current-properties attr) '())
      (with new-vals (list-difference old-vals (list val))
        (when (or (!= attr "owner") (nnull? new-vals))
          (set! current-properties
                (assoc-set! current-properties attr new-vals)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (client-properties-list)
  (aligned
    (item (text "name:")
      (input (if answer (assoc-set! current-properties "name" (list answer)))
             "string" (assoc-ref current-properties "name") "30em"))
    (for (kv current-properties)
      (item (text (string-append (car kv) ":"))
        (horizontal
          (for (val (cdr kv))
            ;;(=> (eval val)
            ;;("Remove" (remove-value (car kv) val)))
            (text val)
            //))))))

(tm-widget (client-properties-editor quit)
  (padded
    (refresh client-properties-list)
    === --- ===
    (form "modify-props"
      (aligned
        (item (text "Attribute:")
          (hlist
            (form-input "attr" "string" (default-attributes) "10em")
            ///
            ("Add" (add-attribute (form-ref "attr")))
            ("Remove" (remove-attribute (form-ref "attr")))))
        (item (text "Value:")
          (hlist
            (form-input "val" "string"
                        (default-values (form-ref "attr"))
                        "10em")
            ///
            ("Add" (add-value (form-ref "attr") (form-ref "val")))
            ("Remove" (remove-value (form-ref "attr") (form-ref "val")))))))
    === ===
    (bottom-buttons
      ("Cancel" (begin (set! current-url (url-none)) (quit)))
      >>
      ("Ok" (begin (set! current-url (url-none)) (quit))))))

(tm-define (has-client-properties? u)
  (set! u (url->string u))
  (and (url-none? current-url)
       (resource-cache-get-all u)))

(tm-define (open-client-properties-editor)
  (:interactive #t)
  (with u (url->string (current-buffer))
    (set! current-url u)
    (set! current-properties (resource-cache-get-all u))
    (when current-properties
      (dialogue-window client-properties-editor noop "Properties editor"))))
