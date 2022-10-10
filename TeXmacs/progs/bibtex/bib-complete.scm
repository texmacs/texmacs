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

(texmacs-module (bibtex bib-complete)
  (:use (utils library ptrees)))

(define parse-times (make-ahash-table))
(define parse-results (make-ahash-table))
(define bib-files-cache (make-ahash-table))
(define bib-styles-cache (make-ahash-table))

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
(tm-define (current-bib-file usecache?)
  (:synopsis "Returns the (cached) name of the bibliography file")
  (with u (current-buffer-url)
    (or (and usecache? (ahash-ref bib-files-cache u))
        (with l (select (buffer-tree) '(:* bibliography))
          (if (nnull? l)
              (ahash-set! bib-files-cache u
               (url-append (url-head u) (tm->string (tree-ref (car l) 2))))
              (url-none))))))

(tm-define (current-bib-style usecache?)
  (:synopsis "Returns the (cached) style of the bibliography")
  (with u (current-buffer-url)
    (or (and usecache? (ahash-ref bib-styles-cache u))
        (with l (select (buffer-tree) '(:* bibliography))
          (if (nnull? l)
              (ahash-set! bib-styles-cache u (tm->string (tree-ref (car l) 1)))
              "tm-plain")))))

(tm-define (citekey-list u s)
  (:synopsis "Completions for @s in the bibtex file @u as a list")
  (if (url-none? u) '()
      (pt-words-below (pt-find (get-citekeys-pt u) s))))

(tm-define (citekey-completions u t)
  (:synopsis "Completions for @t in the bibtex file @u for custom-complete")
  `(tuple ,t
     ,@(map string->tmstring (citekey-list u (tree->string t)))))
