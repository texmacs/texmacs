
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
  (ahash-set! resource-cache u props)
  (when (buffer-exists? u)
    (with title (resource-cache-get-first u "name" "Nameless remote file")
      (buffer-set-title u title))))

(tm-define (resource-cache-get-all u)
  (ahash-ref resource-cache u))

(tm-define (resource-cache-get u attr)
  (or (assoc-ref (resource-cache-get-all u) attr) '()))

(tm-define (resource-cache-get-first u attr default)
  (with vals (resource-cache-get u attr)
    (if (null? vals) default (car vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties on the server side
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-set-properties u props)
  (with fname (url->string u)
    (when (string-starts? fname "tmfs://remote-file/")
      (let* ((name (substring fname 19 (string-length fname)))
             (sname (tmfs-car name))
             (server (client-find-server sname)))
        (if (not server)
            (texmacs-error "client-set-properties" "invalid server")
            (begin
              (client-remote-eval server `(remote-set-properties ,name ,props)
                (lambda (new-props)
                  (resource-cache-set-all fname new-props))
                (lambda (err)
                  (set-message err "set remote properties")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-url (url-none))
(define current-properties (list))
(define current-attribute "")

(define (attribute<=? a1 a2)
  (cond ((== a1 "date") #t)
        ((== a2 "date") #f)
        ((== a1 "owner") #t)
        ((== a2 "owner") #f)
        ((== a1 "readable") #t)
        ((== a2 "readable") #f)
        ((== a1 "writable") #t)
        ((== a2 "writable") #f)
        (else (string<=? a1 a2))))

(define (set-properties props)
  (with leq? (lambda (x1 x2) (attribute<=? (car x1) (car x2)))
    (set! current-properties (sort props leq?))))

(define (default-attributes)
  ;;(map car current-properties)
  (list "" "owner" "readable" "writable" "keywords"))

(define (default-values attr)
  (or (assoc-ref current-properties attr) '()))

(define (client-reserved-attributes)
  (list "" "name" "type" "location" "date"))

(define (add-attribute attr)
  (when (and attr (not (assoc-ref current-properties attr)))
    (set-properties (assoc-set! current-properties attr '()))))

(define (add-value attr val)
  (when (and attr val (nin? attr (client-reserved-attributes)))
    (with old-vals (or (assoc-ref current-properties attr) '())
      (when (nin? val old-vals)
        (with new-vals (rcons old-vals val)
          (set-properties (assoc-set! current-properties attr new-vals)))))))

(define (remove-attribute attr)
  (when (and attr (nin? attr (cons "owner" (client-reserved-attributes))))
    (set-properties (assoc-remove! current-properties attr))))

(define (remove-value attr val)
  (when (and attr val (nin? attr (client-reserved-attributes)))
    (with old-vals (or (assoc-ref current-properties attr) '())
      (with new-vals (list-difference old-vals (list val))
        (when (or (!= attr "owner") (nnull? new-vals))
          (set-properties (assoc-set! current-properties attr new-vals)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (client-properties-list)
  (aligned
    (item (text "name:")
      (input (if answer (assoc-set! current-properties "name" (list answer)))
             "string" (assoc-ref current-properties "name") "30em"))
    (for (attr (list-difference (map car current-properties)
                                (client-reserved-attributes)))
      (item (text (string-append attr ":"))
        (horizontal
          (for (val (assoc-ref current-properties attr))
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
            (form-input "attr" "string" (default-attributes) "1w")
            ///
            ("Add" ;((balloon (icon "tm_add.xpm") "Add attribute")
             (add-attribute (form-ref "attr")))
            ("Remove" ;(balloon (icon "tm_focus_delete.xpm") "Remove attribute")
             (remove-attribute (form-ref "attr")))))
        (item (text "Value:")
          (hlist
            (form-input "val" "string" '() "1w")
            ///
            ("Add" ;(balloon (icon "tm_add.xpm") "Add value")
             (add-value (form-ref "attr") (form-ref "val")))
            ("Remove" ;(balloon (icon "tm_focus_delete.xpm") "Remove value")
             (remove-value (form-ref "attr") (form-ref "val")))))))
    === ===
    (bottom-buttons
      ("Cancel" (begin
                  (set! current-url (url-none))
                  (quit)))
      >>
      ("Ok" (begin
              (client-set-properties current-url current-properties)
              (set! current-url (url-none))
              (quit))))))

(tm-define (has-client-properties? u)
  (set! u (url->string u))
  (and (url-none? current-url)
       (resource-cache-get-all u)))

(tm-define (open-client-properties-editor)
  (:interactive #t)
  (with u (url->string (current-buffer))
    (set! current-url u)
    (set! current-properties (resource-cache-get-all u))
    (when (and current-properties (assoc-ref current-properties "type"))
      (let* ((type (upcase-first (car (assoc-ref current-properties "type"))))
             (title (string-append type " properties")))
      (dialogue-window client-properties-editor noop title)))))
