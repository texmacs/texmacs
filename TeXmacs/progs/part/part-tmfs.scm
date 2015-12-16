
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
  (:use (part part-shared)
        (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (part-master name)
  (with pos (string-search-forwards ".tm/" 0 name)
    (if (<= pos 0)
        (tmfs-string->url name)
        (tmfs-string->url (substring name 0 (+ pos 3))))))

(tm-define (part-file name)
  (with pos (string-search-forwards ".tm/" 0 name)
    (if (<= pos 0)
        (tmfs-string->url name)
        (let* ((m (tmfs-string->url (substring name 0 (+ pos 3))))
               (s (substring name (+ pos 4) (string-length name)))
               (f (tmfs-string->url s)))
          (if (string-starts? s "here/")
              (url-relative m f)
              f)))))

(tm-define (part-open-name u)
  (with s (url->string u)
    (if (string-starts? s "tmfs://part/")
	(string-drop s (string-length "tmfs://part/"))
	(url->tmfs-string u))))

(tm-define (part-url master file)
  (cond ((== master file)
         (string-append "tmfs://part/" (url->tmfs-string master)))
        ((url-descends? file (url-head master))
         (string-append "tmfs://part/" (url->tmfs-string master)
                        "/" (url->tmfs-string (url-delta master file))))
        (else
         (string-append "tmfs://part/" (url->tmfs-string master)
                        "/" (url->tmfs-string file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for managing the initial environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unpack-extra-inits l)
  (if (or (null? l) (null? (cdr l))) (list)
      (cons `(associate ,(car l) ,(cadr l))
            (unpack-extra-inits (cddr l)))))

(define (get-extra-init t delta)
  (cons `(associate "part-flag" "true")
        (if (and (tm-func? t 'tuple)
                 (tm-equal? (tm-ref t 0) delta))
            (unpack-extra-inits (cdr (tm-children t)))
            (list))))

(define (exclude-from-inherit)
  (list "preamble" "mode"
        "page-medium" "page-printed" "page-first"))

(define (master-inits mas u m)
  (let* ((mt (or (tmfile-extract mas 'initial) `(collection)))
         (xt (collection-exclude mt (exclude-from-inherit)))
         (aux (tmfile-extract mas 'auxiliary))
         (parts (collection-ref aux "parts"))
         (delta (url->unix (url-delta m u)))
         (l (if (tm-func? parts 'document) (tm-children parts) (list)))
         (xinit (append-map (cut get-extra-init <> delta) l))
         (t (collection-append xt `(collection ,@xinit)))
         (refs (tmfile-extract mas 'references))
         (lab (string-append "part:" delta))
         (ref (and refs (collection-ref refs lab))))
    (when (and ref (tm-func? ref 'tuple)
               (tm-ref ref 1)
               (tm-atomic? (tm-ref ref 1))
               (string-number? (tm->string (tm-ref ref 1))))
      (set! t (collection-set t "page-first"
                              (tm->string (tm-ref ref 1)))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-title-handler (part name doc)
  (let* ((m (part-master name))
         (f (part-file name)))
    (if (== m f)
        (url->unix (url-basename (url-tail m)))
        (string-append (url->unix (url-basename (url-tail m))) " - "
                       (if (url-rooted-tmfs? f)
                           (tmfs-title f doc)
                           (url->unix (url-basename (url-tail f))))))))

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
         (let* ((mt (master-inits mas u m))
                (ft (tm-ref doc 0))
                (t  (collection-append mt ft)))
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
  (let* ((m (part-master name))
         (f (part-file name))
         (doc (tree-import f "texmacs"))
         (mas (if (== m f) doc (tree-import m "texmacs")))
         (exp (part-expand doc mas f m)))
    (tmfs-document exp)))

(tmfs-master-handler (part name)
  (part-file name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-compress-body doc u)
  (cond ((tm-func? doc 'document)
         (cons (tm-label doc)
               (map (cut part-compress-body <> u) (tm-children doc))))
        ((tm-func? doc 'shared 3)
         (let* ((name  (tm->string (tm-ref doc 1)))
                (delta (url-delta u name)))
           `(include ,(url->unix delta))))
        (else doc)))

(define (part-compress doc ori mas u m)
  (cond ((tm-atomic? doc) doc)
        ((tm-func? doc 'body 1)
         (with body (tm-ref doc 0)
           (when (and (!= u m) (tm-func? body 'shared))
             (set! body (tm-ref body :last)))
           (when (and (!= u m) (tm-func? body 'document 1)
                      (tm-func? (tm-ref body 0) 'shared))
             (set! body (tm-ref body 0 :last)))
           `(body ,(part-compress-body body u))))
        ((tm-in? doc '(style references auxiliary))
         (with val (tmfile-extract ori (tm-label doc))
           (cond (val `(,(tm-label doc) ,val))
                 ((tm-is? doc 'style) doc)
                 (else `(,(tm-label doc) ,(assoc->collection (list)))))))
        ((tm-is? doc 'initial)
         (let* ((mt (master-inits mas u m))
                (ft (tm-ref doc 0))
                (t  (collection-delta mt ft)))
           `(initial ,t)))
        ((tm-is? doc 'document)
         (cons 'document
               (map (cut part-compress <> ori mas u m)
                    (tm-children doc))))
        (else doc)))

(tmfs-save-handler (part name doc)
  (let* ((m (part-master name))
         (f (part-file name))
         (ori (tree-import f "texmacs"))
         (mas (if (== m f) doc (tree-import m "texmacs")))
         (com (part-compress doc ori mas f m)))
    ;;(display* "com= " (tm->stree com) "\n")
    (if (tree-export (tm->tree com) f "texmacs")
        (buffer-pretend-modified f)
        (buffer-pretend-saved f))))

(tmfs-wrap-handler (part name)
  (part-file name))
