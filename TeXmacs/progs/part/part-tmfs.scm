
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : part-tmfs.scm
;; DESCRIPTION : parts of documents as documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (part part-tmfs)
  (:use (part part-shared)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-master u)
  (and (url-concat? u)  
       (or (part-master (url-head u))
           (and (== (url-suffix u) "tm") u))))

(define (part-parent u)
  (and (url-concat? u)
       (if (== (url-suffix (url-head u)) "tm")
           (url-head u)
           (part-parent (url-head u)))))

(define (part-delta u)
  (and (url-concat? u)
       (if (== (url-suffix (url-head u)) "tm")
           (url-tail u)
           (and-with d (part-delta (url-head u))
             (url-append d (url-tail u))))))

(define (part-file u)
  (let* ((m (part-master u))
         (p (part-parent u))
         (d (part-delta u)))
    (if (== u m) u (url-relative (part-file p) d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-title u)
  (cond ((url-concat? u)
         (let* ((h (part-title (url-head u)))
                (t (part-title (url-tail u))))
           (if (and h t) (string-append h " - " t)
               (or h t))))
        ((url-atomic? u)
         (and (== (url-suffix u) "tm")
              (url->string (url-basename u))))
        (else #f)))

(tmfs-title-handler (part name doc)
  (if (string-ends? name "/") (set! name (string-append name "x")))
  (with u (tmfs-string->url name)
    (or (part-title u)
        (url->system (url-tail u)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-shared u body)
  (let* ((id (create-unique-id))
         (fid (url->unix u)))
    `(shared ,id ,fid ,(tm->stree body))))

(define (part-expand-body doc u)
  (cond ((tm-func? doc 'document)
         (cons (tm-label doc)
               (map (cut part-expand-body <> u) (tm-children doc))))
        ((and (or (tm-func? doc 'include 1)
                  (tm-func? doc 'include* 1))
              (tm-atomic? (tm-ref doc 0)))
         (let* ((su (url-relative u (tm->string (tm-ref doc 0))))
                (sdoc (tree-import su "texmacs"))
                (sbody (tmfile-extract sdoc 'body)))
           (make-shared su sbody)))
        (else doc)))

(define (unpack-extra-inits l)
  (if (or (null? l) (null? (cdr l))) (list)
      (cons `(associate ,(car l) ,(cadr l))
            (unpack-extra-inits (cddr l)))))

(define (get-extra-init t delta)
  (if (and (tm-func? t 'tuple)
           (tm-equal? (tm-ref t 0) delta))
      (unpack-extra-inits (cdr (tm-children t)))
      (list)))

(define (exclude-from-inherit)
  (list "preamble" "mode"
        "page-medium" "page-printed" "page-first"))

(define (part-expand doc mas u m)
  (cond ((tm-atomic? doc) doc)
        ((tm-func? doc 'body 1)
         (with body (part-expand-body (tm-ref doc 0) u)
           (if (!= u m) (set! body (make-shared u body)))
           `(body ,body)))
        ((tm-in? doc '(style references auxiliary))
         (with val (tmfile-extract mas (tm-label doc))
           (if val `(,(tm-label doc) ,val) doc)))
        ((tm-is? doc 'initial)
         (let* ((mt (tmfile-extract mas 'initial))
                (xt (collection-exclude mt (exclude-from-inherit)))
                (ft (tm-ref doc 0))
                (jt (if xt (collection-append xt ft) ft))
                (aux (tmfile-extract mas 'auxiliary))
                (parts (collection-ref aux "parts"))
                (delta (url->unix (url-delta m u)))
                (l (if (tm-func? parts 'document) (tm-children parts) (list)))
                (xinit (append-map (cut get-extra-init <> delta) l))
                (t (collection-append jt `(collection ,@xinit)))
                (refs (tmfile-extract mas 'references))
                (lab (string-append "part:" delta))
                (ref (and refs (collection-ref refs lab))))
           (when (and ref (tm-func? ref 'tuple)
                      (tm-ref ref 1)
                      (tm-atomic? (tm-ref ref 1))
                      (string-number? (tm->string (tm-ref ref 1))))
             (set! t (collection-set t "page-first"
                                     (tm->string (tm-ref ref 1)))))
           `(initial ,t)))
        ((tm-is? doc 'document)
         (when (and (tmfile-extract doc 'body)
                    (not (tmfile-extract doc 'initial)))
           (set! doc (tmfile-assign doc 'initial (assoc->collection (list)))))
         (cons 'document
               (map (cut part-expand <> mas u m)
                    (tm-children doc))))
        (else doc)))

(tmfs-load-handler (part name)
  (if (string-ends? name "/") (set! name (string-append name "x")))
  (let* ((u (tmfs-string->url name))
         (m (part-master u))
         (f (part-file u))
         (doc (tree-import f "texmacs"))
         (mas (if (== m f) doc (tree-import m "texmacs")))
         (exp (part-expand doc mas f m)))
    (if (and m f (not (string-ends? name "/x")))
        (tmfs-document exp)
        ($generic "Invalid file."))))

(tmfs-master-handler (part name)
  (if (string-ends? name "/") (set! name (string-append name "x")))
  (let* ((u (tmfs-string->url name))
         (f (part-file u)))
    (or f name)))
