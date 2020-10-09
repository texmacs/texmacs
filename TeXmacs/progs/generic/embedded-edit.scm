
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : embedded-edit.scm
;; DESCRIPTION : routines for managing embedded and linked images
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic embedded-edit)
  (:use (utils library tree)
        (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (image-context? t)
  (and t (tm-func? t 'image 5)))

(tm-define (embedded-image-context? t)
  (and (image-context? t)
       (tm-is? (tm-ref t 0) 'tuple)
       (tm-is? (tm-ref t 0 0) 'raw-data)))

(tm-define (linked-image-context? t)
  (and (image-context? t)
       (not (embedded-image-context? t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage embedded images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (embedded-suffix t)
  (and (embedded-image-context? t)
       (let* ((f (tm->string (tm-ref t 0 1)))
              (s (url-suffix f)))
         (if (== s "") f s))))

(tm-define (embedded-propose t nr)
  (and (embedded-image-context? t)
       (let* ((f (tm->string (tm-ref t 0 1)))
              (s (url-suffix f))
              (c (current-buffer))
              (r (url->string (url-basename (url-tail c))))
              (d (string-append r "-image-" (number->string nr) "." f))
              (n (if (== s "") d f)))
         (url->string (url-relative c n)))))

(tm-define (save-embedded-image t name)
  (when (embedded-image-context? t)
    (string-save (tm->string (tm-ref t 0 0 0)) name)))

(tm-define (link-embedded-image t name)
  (when (embedded-image-context? t)
    (save-embedded-image t name)
    (with rel (url->string (url-delta (current-buffer) name))
      (tree-set! t 0 rel))))

(tm-define (link-embedded-image-copies t name)
  (when (embedded-image-context? t)
    (save-embedded-image t name)
    (let* ((rel (url->string (url-delta (current-buffer) name)))
           (orig (tree-copy (tree-ref t 0))))
      (tree-replace (buffer-tree) (cut == <> orig)
                    (lambda (c) (tree-set! c rel))))))

(tm-define (embedded-saver name)
  (with t (tree-innermost embedded-image-context? #t)
    (save-embedded-image t name)))
(tm-define (save-embedded-image-as)
  (:interactive #t)
  (let* ((t (tree-innermost embedded-image-context? #t))
         (s (embedded-suffix t))
         (p (embedded-propose t 1)))
    (choose-file embedded-saver "Save embedded image" s "Save" p)))

(tm-define (embedded-linker name)
  (with t (tree-innermost embedded-image-context? #t)
    (link-embedded-image t name)))
(tm-define (link-embedded-image-as)
  (:interactive #t)
  (let* ((t (tree-innermost embedded-image-context? #t))
         (s (embedded-suffix t))
         (p (embedded-propose t 1)))
    (choose-file embedded-linker "Link embedded image" s "Save" p)))

(tm-define (embedded-linker-copies name)
  (with t (tree-innermost embedded-image-context? #t)
    (link-embedded-image-copies t name)))
(tm-define (link-embedded-image-copies-as)
  (:interactive #t)
  (let* ((t (tree-innermost embedded-image-context? #t))
         (s (embedded-suffix t))
         (p (embedded-propose t 1)))
    (choose-file embedded-linker-copies "Link embedded image and copies"
                 s "Save" p)))

(define (strip-suffix u)
  (with suffix (url-suffix u)
    (if (== suffix "") u
        (with r (url-unglue u (+ (string-length suffix) 1))
          (if (string? u) (url->string r) r)))))

(define (url-number u nr)
  (with num (string-append "-" (number->string nr))
    (if (== (url-suffix u) "")
        (url-glue u num)
        (url-glue (strip-suffix u) (string-append num "." (url-suffix u))))))

(define (url-free u nr)
  (cond ((not (url-exists? u)) u)
        ((not (url-exists? (url-number u nr))) (url-number u nr))
        (else (url-free u (+ nr 1)))))

(define (embedded-list t)
  (let* ((tl (tree-search t embedded-image-context?))
         (il (... 1 (length tl)))
         (fl (map embedded-propose tl il)))
    (map list tl fl)))

(tm-define (save-all-embedded-images)
  (for (p (embedded-list (buffer-tree)))
    (with (t u) p
      (save-embedded-image t (url-free u 2)))))

(tm-define (link-all-embedded-images)
  (for (p (embedded-list (buffer-tree)))
    (with (t u) p
      (link-embedded-image t (url-free u 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage linked images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (embed-image t)
  (when (and (linked-image-context? t) (tree-atomic? (tree-ref t 0)))
    (let* ((f (tm->string (tm-ref t 0)))
           (u (url-relative (current-buffer) f))
           (s (url-suffix f)))
      (when (url-exists? u)
        (let* ((data (string-load u))
               (raw `(tuple (raw-data ,data) ,(url->string (url-tail f)))))
          (tree-set t 0 raw))))))

(tm-define (embed-images t)
  (cond ((tree-atomic? t) (noop))
        ((linked-image-context? t) (embed-image t))
        (else (for-each embed-images (tree-children t)))))

(tm-define (embed-this-image)
  (with t (tree-innermost linked-image-context? #t)
    (embed-image t)))

(tm-define (embed-all-images)
  (embed-images (buffer-tree)))
