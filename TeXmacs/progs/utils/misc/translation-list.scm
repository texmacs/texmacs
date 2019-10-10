
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : translation-list.scm
;; DESCRIPTION : determine list of GUI elements to be translated
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc translation-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translatable? x)
  (or (string? x)
      (func? x 'concat)
      (func? x 'verbatim)
      (func? x 'replace)))

(define (menu-label? x)
  (or (translatable? x)
      (func? x 'color)
      (func? x 'icon)
      (and (func? x 'extend) (pair? (cdr x))
           (menu-label? (cadr x)))
      (and (func? x 'style 2)
           (menu-label? (caddr x)))
      (and (func? x 'balloon 2)
           (menu-label? (cadr x)) (translatable? (caddr x)))))

(define (menu-wide-label? x)
  (or (menu-label? x)
      (and (func? x 'check 3)
           (menu-wide-label? (cadr x)))
      (and (func? x 'shortcut 2)
           (menu-wide-label? (cadr x)))))

(define (menu-item-cmd-0? x)
  (and (pair? x)
       (in? (car x) '(horizontal vertical hlist vlist aligned aligned-item
                      item meti tabs tab icon-tabs icon-tab
                      minibar extend scrollable hsplit vsplit
                      inert explicit-buttons bold grey monospaced
                      padded centered))))

(define (menu-item-cmd-1? x)
  (and (pair? x) (pair? (cdr x))
       (in? (car x) '(-> => style tile refreshable
                     if when mini let let*))))

(define (menu-item-cmd-2? x)
  (and (pair? x) (pair? (cdr x))
       (in? (car x) '(resize with))))

(define (menu-item-text-1? x)
  (and (list-2? x)
       (in? (car x) '(group text))
       (string? (cadr x))))

(define (menu-item-list-2? x)
  (and (func? x 'enum 4)
       (list? (caddr x))
       (forall? string? (caddr x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find translatable items in a menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-string s t)
  (with n (string-length s)
    (cond ((<= (string-length s) 1) (noop))
          ((and (not (string-alpha? (substring s 0 1)))
                (nin? (substring s 0 1) (list "(")))
           (add-string (substring s 1 n) t))
          ((and (not (string-alpha? (substring s (- n 1) n)))
                (nin? (substring s (- n 1) n) (list ")")))
           (add-string (substring s 0 (- n 1)) t))
          (else
           (ahash-set! t (locase-first s) "")))))

(define (search-translatable-label x t)
  (cond ((string? x)
         (add-string x t))
        ((and (pair? x) (in? x '(extend check shortcut)))
         (search-translatable-label (cadr x) t))
        ((func? x 'style)
         (search-translatable-label (caddr x) t))
        ((func? x 'balloon)
         (search-translatable-label (cadr x) t)
         (search-translatable-label (caddr x) t))))

(define (search-translatable-menu-one x t)
  ;;(display* "x= " x "\n")
  (cond ((or (nlist? x) (npair? x)) (noop))
        ((menu-wide-label? (car x))
         (search-translatable-label (car x) t))
        ((in? (car x) '(-> =>))
         (when (and (pair? (cdr x)) (menu-wide-label? (cadr x)))
           (search-translatable-label (cadr x) t))
         (search-translatable-menu (cddr x) t))
        ((menu-item-cmd-0? x)
         (search-translatable-menu (cdr x) t))
        ((menu-item-cmd-1? x)
         (search-translatable-menu (cddr x) t))
        ((menu-item-cmd-2? x)
         (search-translatable-menu (cdddr x) t))
        ((menu-item-text-1? x)
         (add-string (cadr x) t))
        ((menu-item-list-2? x)
         (for (s (caddr x))
           (add-string s t)))))

(define (search-translatable-menu l t)
  (when (list? l)
    (for-each (cut search-translatable-menu-one <> t) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find translatable items in a scheme expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-translatable-file-one x t)
  (cond ((or (nlist? x) (npair? x)) (noop))
        ((and (in? (car x) '(menu-bind tm-menu tm-widget))
              (pair? (cdr x)))
         (with l (cddr x)
           (while (and (pair? l) (pair? (car l)) (keyword? (caar l)))
             (set! l (cdr l)))
           (search-translatable-menu l t)))))

(define (search-translatable-file-bis x t)
  (cond ((nlist? x) (noop))
        ((func? x 'set-message 2)
         (when (string? (cadr x))
           (add-string (cadr x) t))
         (when (string? (caddr x))
           (add-string (caddr x) t)))
        (else (for-each (cut search-translatable-file-bis <> t) x))))

(define (search-translatable-file x t)
  ;;(when (url? x) (display* "x= " x "\n"))
  (cond ((and (url? x) (or (url-or? x) (url-none? x)))
         (for-each (cut search-translatable-file <> t) (url->list x)))
        ((and (url? x) (url-directory? x))
         (with u (url-complete (url-append x (url-wildcard "*")) "r")
           (search-translatable-file (url-expand u) t)))
        ((url? x)
         (when (and (url-exists? x) (== (url-suffix x) "scm"))
           (with s (string-load x)
             (with p (string->object (string-append "(\n" s "\n)"))
               (search-translatable-file p t)))))
        ((list? x)
         (for-each (cut search-translatable-file-one <> t) x)
         (for-each (cut search-translatable-file-bis <> t) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage files with translations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tr-file language)
  (url-concretize
   (string-append "$TEXMACS_PATH/langs/natural/dic/english-"
                  language
                  ".scm")))

(define (tr-miss language)
  (url-concretize
   (string-append "$TEXMACS_PATH/langs/natural/miss/english-"
                  language
                  "-miss.scm")))

(define (tr-auto language)
  (url-concretize
   (string-append "$TEXMACS_PATH/langs/natural/miss/list-"
                  language
                  ".scm")))

(define (tr-new) (tr-file "new"))
(define (tr-extra) (tr-file "extra"))
(define (tr-ignore) (tr-file "ignore"))

(define (tr-load u)
  (with t (make-ahash-table)
    (when (url-exists? u)
      (with s (string-append "(\n" (string-load u) "\n)\n")
        (for (x (string->object s))
          (when (list-2? x)
            (ahash-set! t (car x) (cadr x))))))
    t))

(define (tr-save u t)
  (let* ((vs (map car (ahash-table->list t)))
         (ws (sort vs string<=?))
         (ps (map (lambda (v) (list v (ahash-ref t v))) ws))
         (ss (map (lambda (p) (string-append (object->string p) "\n")) ps))
         (s  (apply string-append ss)))
    (string-save s u)))

(define (tr-search u)
  (with t (make-ahash-table)
    (search-translatable-file u t)
    t))

(define (ahash-table-filter t pred?)
  (let* ((l (ahash-table->list t))
         (f (list-filter l pred?))
         (u (list->ahash-table f)))
    u))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further routines for speeding up translations using on-line tools
;;   1) First call 'translate-begin' for your language.
;;      This creates a file $TEXMACS_PATH/langs/natural/miss/list-english.scm
;;   2) Next translate this file using an online tool.
;;      Put the result in $TEXMACS_PATH/langs/natural/miss/list-[language].scm
;;   3) Call 'translate-end' for your language.
;;      The english and translated lists will be combined and
;;      the file with missing translations will be updated accordingly.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (translate-begin language)
  (let* ((mt (tr-load (tr-miss language)))
         (ml (sort (map car (ahash-table->list mt)) string<=?))
         (nl (map (lambda (i) (list (+ i 1) (list-ref ml i)))
                  (.. 0 (length ml))))
         (ss (map (lambda (p) (string-append (object->string p) "\n")) nl))
         (s  (apply string-append ss)))
    (string-save s (tr-auto "english"))))

(tm-define (translate-end language)
  (let* ((mt (tr-load (tr-miss language)))
         (in (tr-load (tr-auto "english")))
         (out (tr-load (tr-auto language)))
         (vs (map car (ahash-table->list out))))
    (for (v vs)
      (and-let* ((eng (ahash-ref in v))
                 (lan (ahash-ref out v)))
        (when (== (ahash-ref mt eng) "")
          (ahash-set! mt eng lan))))
    (tr-save (tr-miss language) mt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (search-translatable u)
  (with t (make-ahash-table)
    (search-translatable-file u t)
    (sort (map car (ahash-table->list t)) string<=?)))

(tm-define (generate-extra-translatable)
  (let* ((nt (tr-load (tr-new)))
         (it (tr-load (tr-ignore)))
         (mt (ahash-table-append nt it))
         (at (tr-search (string->url "$TEXMACS_PATH/progs")))
         (xt (ahash-table-difference at mt)))
    (tr-save (tr-extra) xt)))

(tm-define (update-translatable)
  (generate-extra-translatable)
  (let* ((nt (tr-load (tr-new)))
         (xt (tr-load (tr-extra)))
         (dt (ahash-table-difference xt nt))
         (at (ahash-table-append nt dt)))
    (tr-save (tr-new) at)))

(tm-define (update-missing-for language)
  (let* ((lt (tr-load (tr-file language)))
         (nt (tr-load (tr-new)))
         (dt (ahash-table-difference nt lt)))
    (tr-save (tr-miss language) dt)))

(tm-define (update-missing)
  (for-each update-missing-for supported-languages))

(tm-define (merge-missing-for language)
  (let* ((lt (tr-load (tr-file language)))
         (mt (tr-load (tr-miss language)))
         (nt (ahash-table-filter mt (lambda (x)  (!= (cdr x) ""))))
         (at (ahash-table-append lt nt))
         (dt (ahash-table-difference mt nt)))
    (when (!= (ahash-size nt) 0)
      (tr-save (tr-file language) at))))

(tm-define (merge-missing)
  (for-each merge-missing-for supported-languages))

(tm-define (save-translations)
  (let* ((opt (cons "texmacs->verbatim:encoding" "cork"))
         (con (convert (buffer-tree) "texmacs-tree" "verbatim-snippet" opt)))
    (string-save con (current-buffer))))
