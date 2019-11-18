
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-kbd.scm
;; DESCRIPTION : keyboard shortcuts for editing bibliographic databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-kbd)
  (:use (database bib-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completing bibliographic references
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-variant t forwards?)
  (:require (and (supports-db?) (bib-cite-context? t)))
  (and-with u (if (tree-func? t 'cite-detail) (tree-ref t 0) (tree-down t))
    (and-with key (and (tree-atomic? u) (tree->string u))
      (with-database (bib-database)
        (with completions (sort (index-get-name-completions key) string<=?)
          (if (null? completions)
              (set-message "No completions" "complete bibliographic reference")
              (with cs (cons key (map (cut string-drop <> (string-length key))
                                      completions))
                (custom-complete (tm->tree `(tuple ,@cs))))))))))

(tm-define (kbd-alternate-variant t forwards?)
  (:require (and (supports-db?) (bib-cite-context? t)))
  (focus-open-search-tool t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform author and editor names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transform-one-name s*)
  (let* ((s (tm-string-trim-both s*))
         (l (cpp-string-tokenize s " ")))
    (cond ((== (length l) 0) "")
          ((== (length l) 1) s)
          (else (with r (cpp-string-recompose (cDr l) " ")
                  `(concat ,(string-append r " ") (name ,(cAr l))))))))

(define (transform-name s)
  (if (nstring? s) s
      (let* ((l1 (cpp-string-tokenize s " and "))
             (l2 (map transform-one-name l1)))
        (cond ((== (length l2) 0) "")
              ((== (length l2) 1) (car l2))
              (else (with r (list-intersperse l2 '(name-sep))
                      (apply tmconcat r)))))))

(define (transform-names t)
  (cond ((string? t) (transform-name t))
        ((tm-func? t 'concat)
         (let* ((l (tm-children t))
                (sc (list-scatter l (cut == <> '(name-sep)) #f))
                (ls (map (lambda (l) (apply tmconcat l))  sc))
                (ns (map transform-name ls))
                (n (list-intersperse ns '(name-sep))))
           (apply tmconcat n)))
        (else t)))

(tm-define (kbd-enter t shift?)
  (:require (and (db-field? t) (not shift?)
                 (in? (db-field-attr t) (list "author" "editor"))))
  (let* ((old (tm->stree (tm-ref t :down)))
         (new (transform-names old)))
    (if (== new old)
        (former t shift?)
        (tree-assign (tm-ref t :down) new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-bib-names?)
  ("space a" " a")
  ("space a n" " an")
  ("space a n d space" (make-name-sep))
  ("," (make-name-sep))
  (", var" ",")
  ("S-F5" (make-name-von))
  ("S-F7" (make-name-jr)))
