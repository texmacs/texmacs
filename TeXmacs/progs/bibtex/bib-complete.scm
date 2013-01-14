;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-complete.scm
;; DESCRIPTION : Autocompletion of bibtex citekeys 
;; COPYRIGHT   : (C) 2013 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO:
;;  - Maybe we should use persistent storage for the parsed files?
;;  - This is pretty ad-hoc. We could provide a better interface and use it
;;    for a simple bibliography browser widget, for instance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (bibtex bib-complete)
  (:use (utils library ptrees)))

(define parse-times (make-ahash-table))
(define parse-results (make-ahash-table))
(define bib-files (make-ahash-table))

(define (get-citekeys-list l)
  (list-fold
   (lambda (entry rest) 
     (if (tm-func? entry 'bib-entry) (cons (caddr entry) rest) rest))
   '() l))

(define (get-citekeys-pt u)
  (let ((mod-time (url-last-modified u))
        (parse-time (or (ahash-ref parse-times u) 0)))
    (if (> mod-time parse-time) ; handy: false also if url invalid
        (begin
          (ahash-set! parse-times u mod-time)
          (ahash-set! parse-results u
           (pt-add-list (make-ptree)
             (get-citekeys-list
              (tree->stree (parse-bib (string-load u))))))))
    (ahash-ref parse-results u)))

; FIXME: if the user changes the bibliography file we still retrieve from cache
(tm-define (current-bib-file)
  (:synopsis "Returns the (cached) name of the bibliography file")
  (with u (current-buffer-url)
    (or (ahash-ref bib-files u)
        (with l (select (buffer-tree) '(bibliography))
          (if (nnull? l)
              (ahash-set! bib-files u
               (url-append (url-head u)
                           (tree->string (tree-ref (car l) 2))))
              (url-none))))))

(tm-define (citekey-completions u root)
  (:synopsis "Completions for @root in the bibtex file @u for custom-complete")
  `(tuple ,root
     ,@(map string->tmstring
            (pt-words-below (pt-find (get-citekeys-pt u) (tree->string root))))))
