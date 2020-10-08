
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : auto-tmfs.scm
;; DESCRIPTION : Handling queries for TeXmacs documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils automate auto-tmfs)
  (:use (utils automate auto-build)))

(tmfs-title-handler (automate name doc)
  (let* ((question (tmfs-car name))
         (file-name (tmfs-cdr name))
         (u (tmfs-string->url file-name)))
    (string-append (url->system (url-tail u)) " ? " question)))

(define (query-escape q)
  ;; FIXME: encodings
  (string-replace q "&#42;" "*"))

(define (query->assoc q)
  (let* ((l (string-decompose q ","))
         (ls (map (cut string-decompose <> "=") l))
         (fls (list-filter ls (lambda (x) (== (length x) 2)))))
    (map (lambda (x) (cons (string->symbol (car x)) (cadr x))) fls)))

(tm-define (auto-load-tree u bindings safe?)
  (with doc (if (buffer-exists? u) (buffer-get u) (tree-import u "generic"))
    (with-global auto-safe-mode? safe?
      (apply build-document (cons doc bindings)))))

(tmfs-load-handler (automate name)
  (let* ((q* (tmfs-car name))
	 (q (query-escape q*))
         (bs (query->assoc q))
         (file-name (tmfs-cdr name))
         (u (tmfs-string->url file-name))
         (safe? (string-starts? (url->unix u) "tmfs://help/")))
    (auto-load-tree u bindings safe?)))

(tm-define (auto-load-aux name u bindings)
  (with doc (auto-load-tree u bindings #t)
    (lazy-initialize-force)
    (cursor-history-add (cursor-path))
    (open-auxiliary name doc)))

(tm-define (auto-load-help name help-file)
  (let* ((base "tmfs://help/article/tm/doc/main/automated/")
         (suffix (string-append "." (ext-language-suffix) ".tm"))
         (loc (string-append base help-file suffix))
         (eng (string-append base help-file ".en.tm"))
         (u (if (url-exists? loc) loc eng)))
    (auto-load-aux "Contextual help" u (list))))
