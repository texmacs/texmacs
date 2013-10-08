
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-edit.scm
;; DESCRIPTION : setting global document properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-edit)
  (:use (utils base environment)
        (utils library length)
        (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document style and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (init-style style)
  (:argument style "Document style")
  (:default  style "generic"))

(tm-property (project-attach master)
  (:argument master "Master file"))

(tm-define (get-style-list)
  (with t (tree->stree (get-style-tree))
    (cond ((string? t) (list t))
          ((and (pair? t) (== (car t) 'tuple)) (cdr t))
          (else (texmacs-error "get-style-list ""invalid style ~S" t)))))

(tm-define (style-category p) p)
(tm-define (style-category-overrides? p q) (== p q))
(tm-define (style-category-precedes? p q) #f)

(tm-define (style-includes? p q) #f)

(tm-define (style-overrides? p q)
  (style-category-overrides? (style-category p) (style-category q)))

(tm-define (style-precedes? p q)
  (style-category-precedes? (style-category p) (style-category q)))

(define (normalize-style-list* l)
  (cond ((null? l) l)
        ((list-find (cdr l) (cut style-overrides? <> (car l)))
         (normalize-style-list* (cdr l)))
        ((list-find (cdr l) (cut style-precedes? <> (car l)))
         (normalize-style-list* (cons (cadr l) (cons (car l) (cddr l)))))
        (else (cons (car l) (normalize-style-list* (cdr l))))))

(define (normalize-style-list** l before)
  (cond ((null? l) l)
        ((list-find before (cut style-includes? <> (car l)))
         (normalize-style-list** (cdr l) (cons (car l) before)))
        (else (cons (car l) (normalize-style-list** (cdr l)
                                                    (cons (car l) before))))))

(define (normalize-style-list l)
  (if (null? l) l
      (cons (car l)
            (normalize-style-list** (normalize-style-list* (cdr l))
                                    (list (car l))))))

(tm-define (set-style-list l)
  (set-style-tree (tm->tree `(tuple ,@(normalize-style-list l)))))

(tm-define (has-style-package? pack)
  (or (in? pack (get-style-list))
      (and (list-find (get-style-list) (cut style-includes? <> pack))
           (not (list-find (get-style-list) (cut style-overrides? <> pack))))))

(tm-define (add-style-package pack)
  (:argument pack "Use package")
  (:check-mark "v" has-style-package?)
  (with l (list-remove-duplicates (append (get-style-list) (list pack)))
    (set-style-list l)))

(tm-define (remove-style-package pack)
  (:argument pack "Use package")
  (with l (list-difference (get-style-list) (list pack))
    (set-style-list l)))

(tm-define (toggle-style-package pack)
  (:argument pack "Toggle package")
  (:check-mark "v" has-style-package?)
  (if (has-style-package? pack)
      (remove-style-package pack)
      (add-style-package pack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preamble mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-preamble?)
  (== (get-env "preamble") "true"))

(tm-define (toggle-preamble)
  (:synopsis "Toggle preamble mode.")
  (:check-mark "v" in-preamble?)
  (let ((new (if (string=? (get-env "preamble") "true") "false" "true")))
    (init-env "preamble" new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default? . vals)
  (if (null? vals)
      #t
      (and (not (init-has? (car vals)))
	   (apply test-default? (cdr vals)))))

(tm-define (init-default . args)
  (:check-mark "*" test-default?)
  (for-each init-default-one args))

(tm-define (get-init-env s)
  (with t (get-init-tree s)
    (cond ((tree-atomic? t) (tree->string t))
          ((and (tree-func? t 'macro 1) (tree-atomic? (tree-ref t 0)))
           (tree->string (tree-ref t 0)))
          (else #f))))

(tm-define (test-init? var val)
  (== (get-init-tree var) (string->tree val)))

(tm-property (init-env var val)
  (:check-mark "*" test-init?))

(tm-define (set-init-env s val)
  (with old (get-init-tree s)
    (if (and (tree-func? old 'macro 1) (not (tm-is? val 'macro)))
        (init-env-tree s `(macro ,val))
        (init-env-tree s val))))

(tm-define (init-interactive-env var)
  (:interactive #t)
  (interactive (lambda (s) (set-init-env var s))
    (list (or (logic-ref env-var-description% var) var) "string"
          (get-init-env var))))

(tm-define (toggle-init-env var)
  (with new (if (== (get-init-env var) "true") "false" "true")
    (init-default var)
    (delayed
      (when (!= new (get-init-env var))
        (set-init-env var new)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (init-language lan)
  (let ((cyrillic-before (in? (tree->stree (get-init-tree "language"))
			      '("bulgarian" "russian" "ukrainian")))
	(cyrillic-after (in? lan '("bulgarian" "russian" "ukrainian")))
	(chinese-before (in? (tree->stree (get-init-tree "language"))
			     '("chinese" "taiwanese")))
	(chinese-after (in? lan '("chinese" "taiwanese")))
	(japanese-before (in? (tree->stree (get-init-tree "language"))
			      '("japanese")))
	(japanese-after (in? lan '("japanese")))
	(korean-before (in? (tree->stree (get-init-tree "language"))
			    '("korean")))
	(korean-after (in? lan '("korean"))))
    (if (and cyrillic-before (not cyrillic-after)) (init-default "font"))
    (if (and chinese-before (not chinese-after)) (init-default "font"))
    (if (and japanese-before (not japanese-after)) (init-default "font"))
    (if (and korean-before (not korean-after)) (init-default "font"))
    (init-env "language" lan)
    (if (and cyrillic-after (not cyrillic-before))
	(init-env "font" "cyrillic"))
    (if (and chinese-after (not chinese-before))
	(init-env "font" "sys-chinese"))
    (if (and japanese-after (not japanese-before))
	(init-env "font" "sys-japanese"))
    (if (and korean-after (not korean-before))
	(init-env "font" "sys-korean"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main page layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-default-page-medium?) (test-default? "page-medium"))
(tm-define (init-default-page-medium)
  (:check-mark "*" test-default-page-medium?)
  (init-default "page-medium")
  (notify-page-change))

(define (test-page-medium? s) (string=? (get-env "page-medium") s))
(tm-define (init-page-medium s)
  (:check-mark "*" test-page-medium?)
  (init-env "page-medium" s)
  (notify-page-change))

(define (test-page-type? s) (string=? (get-env "page-type") s))
(tm-define (init-page-type s)
  (:check-mark "*" test-page-type?)
  (init-env "page-type" s)
  (init-default "page-width" "page-height")
  (notify-page-change))

(tm-define (init-page-size w h)
  (:argument w "Page width")
  (:argument h "Page height")
  (init-env "page-type" "user")
  (init-env "page-width" w)
  (init-env "page-height" h))

(define (test-default-page-orientation?) (test-default? "page-orientation"))
(tm-define (init-default-page-orientation)
  (:check-mark "*" test-default-page-orientation?)
  (init-default "page-orientation")
  (notify-page-change))

(define (test-page-orientation? s) (string=? (get-env "page-orientation") s))
(tm-define (init-page-orientation s)
  (:check-mark "*" test-page-orientation?)
  (init-env "page-orientation" s)
  (notify-page-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further page layout settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visible-header-and-footer?)
  (== (get-env "page-show-hf") "true"))

(tm-define (toggle-visible-header-and-footer)
  (:synopsis "Toggle visibility of headers and footers in 'page' paper mode.")
  (:check-mark "v" visible-header-and-footer?)
  (init-env "page-show-hf"
	    (if (== (get-env "page-show-hf") "true") "false" "true")))

(define (page-width-margin?)
  (== (get-env "page-width-margin") "true"))

(tm-define (toggle-page-width-margin)
  (:synopsis "Toggle mode for determining margins from paragraph width.")
  (:check-mark "v" page-width-margin?)
  (init-env "page-width-margin" (if (page-width-margin?) "false" "true")))

(define (not-page-screen-margin?)
  (== (get-env "page-screen-margin") "false"))

(tm-define (toggle-page-screen-margin)
  (:synopsis "Toggle mode for using special margins for screen editing.")
  (:check-mark "v" not-page-screen-margin?)
  (init-env "page-screen-margin"
	    (if (not-page-screen-margin?) "true" "false")))
