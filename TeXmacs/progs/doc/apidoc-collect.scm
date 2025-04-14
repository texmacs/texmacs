;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : apidoc-collect.scm
;; DESCRIPTION : Collect documentation from the manuals.
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Given a page from the documentation, we parse it and all its children and
;; store every "explain" tag into a cache in the users $TEXMACS_HOMEPATH
;; directory. 
;; We use this to provide documentation for scheme symbols and texmacs macros.
;; Multiple languages can be stored for each tag, allowing for localization
;; as well as a fallback language.
;;
;; The format for entries in the cache is
;; ((entry "key" "language" "url/of/original/doc" (stree with doc))
;;  (entry "key" "language2" "anotherurl" (stree with doc))
;;  ...)
;;
;; TODO:
;;  - More robust parsing of 'explain tags:
;;    - do explain-scm?, explain-macro?, etc. always work?
;;  - Add all root paths to manuals in doc-collect-all
;;  - Complement this page based approach with a simple recursive traversal
;;    of all subdirectories and use this to warn about pages not referenced
;;    (i.e. linked with <branch>) anywhere in the manuals.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc apidoc-collect)
    (:use (prog scheme-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables and generic one-use routines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define _scm_ #f)   ; for faster access than doc-scm-cache
(define _macro_ #f) ; for faster access than doc-macro-cache

(define (func-remove l what)
  "Purge all sub-strees of type @what in @l."
  (cond ((null? l) '())
        ((nlist? l) l)
        ((func? l what) '())
        (else (cons (func-remove (car l) what) (func-remove (cdr l) what)))))

(define (flatten-strings t)
  "Return a string with all the strings in the stree @t."
  (cond ((string? t) t)
        ((or (null? t) (nlist? t)) "")
        (else (string-append (flatten-strings (car t))
                             (flatten-strings (cdr t))))))

(define (first-symbol s)
  "Return the first scheme symbol in a string of characters."
  (let* ((beg (string-skip s char-set:stopmark))
         (end (string-skip s (char-set-complement char-set:stopmark) beg)))
    (if (or (not (integer? beg)) (not (integer? end)))
        s (substring s beg end))))

(define (doctree-lan t)
  "Returns the language of the TeXmacs document tree @t."
  (let* ((s (select t '(initial collection associate)))
         (flt (lambda (x) (== (tm-ref x 0) "language")))
         (s2 (list-filter (map tree->stree s) flt)))
    (or (and (nnull? s2) (tm-ref (car s2) 1)) "english")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing and processing of explain tags in texmacs trees.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-scm-cache)
  (:synopsis "Url of the cache with the collected scheme documentation")
  (with pref (get-preference "doc:doc-scm-cache")
    (if (and (!= pref "default") (url-exists? (system->url pref)))
        (system->url pref)
        (with new (persistent-file-name
                   (unix->url "$TEXMACS_HOME_PATH/system/cache/")
                   "api")
          (set-preference "doc:doc-scm-cache" (url->system new))
          new))))

(tm-define (doc-macro-cache)
  (:synopsis "Url of the cache with the collected macro documentation.")
  (with pref (get-preference "doc:doc-macro-cache")
    (if (and (!= pref "default") (url-exists? (system->url pref)))
        (system->url pref)
        (with new (persistent-file-name 
                   (unix->url "$TEXMACS_HOME_PATH/system/cache/")
                   "api")
          (set-preference "doc:doc-macro-cache" (url->system new))
          new))))

(define (explain-scm? t)
  "Is the tree an explain macro for some scheme routine(s)?"
  (nnull? (select t '(0 :* scm)))) ; always OK?

(define (explain-macro? t)
  "Is the tree an explain macro for some texmacs macro(s)?"
  (nnull? (select t '(0 0 :* explain-macro)))) ; always OK?

(define (explain-scm-keywords t)
  "Returns the list of scheme keywords described in an explain tag."
  (with tags (select t '(0 :* scm))
    (map (lambda (x) (first-symbol (tmstring->string (flatten-strings x))))
         tags)))

(define (explain-macro-keywords t)
  "Returns the list of macro names described in an explain tag."
  (with tags (select t '(0 :* explain-macro))
    (map (lambda (x) (tmstring->string (tm-ref x 0))) tags)))

(define (process-explain-sub key cache t lan url)
  "Actually store the tree @t from file @url as @key in @cache."
  ;(display* "persist-set of: " key "\n")
  (with prev (string->object (persistent-get cache key))
    (if (eof-object? prev) (set! prev '()))
    (persistent-set cache key
      (object->string (cons `(entry ,key ,lan ,(url->system url) ,t) prev)))))

(define (process-explain t lan url)
  "Store an explain macro from a given URL into the cache."
  (cond ((explain-scm? t)
         (for-each (lambda (x) (process-explain-sub x _scm_ t lan url)) 
                   (explain-scm-keywords t))
         t)
        ((explain-macro? t)
         (for-each (lambda (x) (process-explain-sub x _macro_ t lan url)) 
                   (explain-macro-keywords t))
         t)
        (else t)))

(define (parse-branch l basedir)
  "Given an stree of type 'branch, open the file and parse it, traversing all
   its children branches in turn. For each 'explain tag, create an entry in
   the cache."
  (if (or (null? l) (not (func? l 'branch))) '()
      ;(begin
      ;(display* "b= " basedir "\n")
      ;(display* "f= " (tm-ref l 1) "\n")
      (let* ((furl (string->url (string-append basedir "/" (tm-ref l 1))))
             (t (tree-import furl "texmacs"))
             (lan (doctree-lan t))
             (ex (map tree->stree (select t '(:* explain))))
             (br (map tree->stree (select t '(:* traverse :* branch)))))
        (set! basedir (string-append basedir "/" (url->system (url-head (tm-ref l 1)))))
        (for-each (lambda (t) (process-explain t lan furl)) ex)
        (for-each (lambda (t) (parse-branch t basedir)) br))));)

;; Define preference defaults in order to have the correct data types

(define (notify-doc-collect-preference pref val)
  (cond ((== pref "doc:collect-timestamp") (noop))
        ((== pref "doc:collect-languages") (noop))
        (else (noop))))

(define-preferences
  ("doc:collect-timestamp" 0 notify-doc-collect-preference)
  ("doc:collect-languages" '() notify-doc-collect-preference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-collect-explains basedir fname)
  (:synopsis "Parse @fname in @basedir and its sub-branches, collecting docs")
  (set! _scm_ (doc-scm-cache))
  (set! _macro_ (doc-macro-cache))
  (parse-branch `(branch (dummy) ,fname) basedir))

(define (doc-collect-sub where what lan)
  (let ((path (string-append "$TEXMACS_PATH/doc/" where "/"))
        (file (string-append what "." lan ".tm"))
        (msg  (string-append  "(" what ", " lan ")")))
    (system-wait "Building index" msg)
    (doc-collect-explains path file)))

(tm-define (doc-collect-all lan)
  (:synopsis "Collect all explain tags available in the documentation")
  (with loc (string-take (language-to-locale lan) 2)
   (doc-collect-sub "devel/scheme" "scheme" loc)
   (doc-collect-sub "devel/plugin" "plugin" loc)
   (doc-collect-sub "devel/plugin" "plugins" loc)
   (doc-collect-sub "devel/source" "source" loc)
   (doc-collect-sub "devel/style" "style" loc)
   (doc-collect-sub "main" "man-reference" loc)
   (set-preference "doc:collect-timestamp" (current-time))
   (append-preference "doc:collect-languages" lan)
   (set-message "Finished collecting symbols documentation."
                (string-append "(" lan ")"))))

(tm-define (doc-check-cache)
  (:synopsis "Ensure that the documentation cache is built")
  (let  ((t (get-preference "doc:collect-timestamp"))
         (lan (get-output-language))
         (langs (get-preference "doc:collect-languages")))
    (if (not (and (!= t "default") (list? langs) (member lan langs)))
        (doc-collect-all lan))))

(define (doc-retrieve* cache key lan)
  (let ((docs (string->object (persistent-get cache key)))
        (aux (lambda (i) (== (tm-ref i 1) lan))))
    (if (eof-object? docs) '() ; (string->object "") => #<eof>
        (with res (list-filter docs aux)
          (if (and (null? res) (!= lan "english")) ; second check just in case 
              (doc-retrieve* cache key "english") 
              res)))))
  
(tm-define (doc-retrieve cache key lan)
  (:synopsis "A list with all help items for @key in language @lan in @cache")
  (doc-check-cache)
  (doc-retrieve* cache key lan))

(define (doc-delete-cache*)
  (with s (url->system (doc-scm-cache))
    (display* "I WOULD HAVE deleted the cache at " s ".\n")
    (reset-preference "doc:doc-scm-cache")
    (set-message `(replace "The cache at %1 was deleted" (verbatim ,s)) ""))
  (with s (url->system (doc-macro-cache))
    (display* "I WOULD HAVE deleted the cache at " s ".\n")
    (reset-preference "doc:doc-macro-cache")
    (set-message `(replace "The cache at %1 was deleted" (verbatim ,s)) ""))
  (reset-preference "doc:collect-timestamp")
  (reset-preference "doc:collect-languages")
  (set! _scm_ (url-none))
  (set! _macro_ (url-none)))

(tm-define (doc-delete-cache)
  (:synopsis "Delete the documentation cache")
  (with run 
      (lambda (go?) (if go? (doc-delete-cache*) (set-message "Cancelled" "")))
    (user-confirm 
        `(replace "All the files at %1 and %2 will be deleted. Are you sure?"
                  (verbatim ,(url->system (doc-scm-cache))) 
                  (verbatim ,(url->system (doc-macro-cache))))
      #t run)))
