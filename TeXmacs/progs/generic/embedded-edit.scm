
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

(tm-define (embedded-propose t)
  (and (embedded-image-context? t)
       (let* ((f (tm->string (tm-ref t 0 1)))
              (s (url-suffix f))
              (c (current-buffer))
              (r (url->string (url-basename (url-tail c))))
              (n (if (== s "") (string-append r "-image." f) f)))
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
         (p (embedded-propose t)))
    (choose-file embedded-saver "Save embedded image" s "Save" p)))

(tm-define (embedded-linker name)
  (with t (tree-innermost embedded-image-context? #t)
    (link-embedded-image t name)))
(tm-define (link-embedded-image-as)
  (:interactive #t)
  (let* ((t (tree-innermost embedded-image-context? #t))
         (s (embedded-suffix t))
         (p (embedded-propose t)))
    (choose-file embedded-linker "Link embedded image" s "Save" p)))

(tm-define (embedded-linker-copies name)
  (with t (tree-innermost embedded-image-context? #t)
    (link-embedded-image-copies t name)))
(tm-define (link-embedded-image-copies-as)
  (:interactive #t)
  (let* ((t (tree-innermost embedded-image-context? #t))
         (s (embedded-suffix t))
         (p (embedded-propose t)))
    (choose-file embedded-linker-copies "Link embedded image and copies"
                 s "Save" p)))
