
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-latex.scm
;; DESCRIPTION : setup latex converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex init-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-recognizes-at? s pos)
  (set! pos (format-skip-spaces s pos))
  (cond ((format-test? s pos "\\document") #t)
	((format-test? s pos "\\usepackage") #t)
	((format-test? s pos "\\input") #t)
	((format-test? s pos "\\includeonly") #t)
	((format-test? s pos "\\chapter") #t)
	((format-test? s pos "\\appendix") #t)
	((format-test? s pos "\\section") #t)
	((format-test? s pos "\\begin") #t)
	(else #f)))

(define (latex-recognizes? s)
  (and (string? s) (latex-recognizes-at? s 0)))

(define-format latex
  (:name "LaTeX")
  (:suffix "tex")
  (:recognize latex-recognizes?))

(define-format latex-class
  (:name "LaTeX class")
  (:suffix "ltx" "sty" "cls"))

(define-preferences
  ("texmacs->latex:transparent-tracking" "on" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs->LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (convert latex texout) serialize-latex)
(lazy-define (convert latex tmtex) texmacs->latex)

(converter texmacs-stree latex-stree
  (:function-with-options texmacs->latex)
  (:option "texmacs->latex:preserve-source" "off")
  (:option "texmacs->latex:replace-style" "on")
  (:option "texmacs->latex:expand-macros" "off")
  (:option "texmacs->latex:expand-user-macros" "off")
  (:option "texmacs->latex:indirect-bib" "off")
  (:option "texmacs->latex:use-macros" "on")
  (:option "texmacs->latex:encoding" "ascii"))

(converter latex-stree latex-document
  (:function serialize-latex))

(converter latex-stree latex-snippet
  (:function serialize-latex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX -> TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-document->texmacs x . opts)
  (if (list-1? opts) (set! opts (car opts)))
  (let*
    ((as-pic   (== (get-preference "latex->texmacs:fallback-on-pictures") "on"))
     (keep-src (== (get-preference "latex<->texmacs:preserve-source") "on")))
    (if (== (get-preference "latex<->texmacs:secure-tracking") "on")
      (secured-latex-document->texmacs x as-pic keep-src)
      (cpp-latex-document->texmacs x as-pic keep-src '()))))

(converter latex-document latex-tree
  (:function parse-latex-document))

(converter latex-snippet latex-tree
  (:function parse-latex))

(converter latex-document texmacs-tree
  (:function-with-options latex-document->texmacs)
  (:option "latex->texmacs:fallback-on-pictures" "on"))

(converter latex-class-document texmacs-tree
  (:function latex-class-document->texmacs))

(converter latex-tree texmacs-tree
  (:function latex->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX -> TeXmacs with secure source tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flatten-document t)
  (cond ((nlist>0? t) t)
        ((not (func? t 'document)) (map flatten-document t))
        (else
          (apply append '(document)
                 (map-in-order
                   (lambda (x)
                     (with y (flatten-document x)
                       (if (func? y 'document) (cdr y) (list y))))
                   (cdr t))))))

(define (remove-creation-date* s)
  (let* ((start (string-search-forwards "%%CreationDate" 0 s))
         (stop  (if (== start -1) 0 (string-search-forwards "\n" start s))))
    (if (== start -1) s
      (string-append (string-take s start) (string-drop s stop)))))

(define (remove-creation-date t)
  ;; Remove "%%CreationDate.*\n" from eps pictures, since it seems impossible
  ;; to prevent Ghostscript from adding it.
  (cond ((func? t 'raw-data) `(raw-data ,(remove-creation-date* (cadr t))))
        ((list>0? t) (map remove-creation-date t))
        (else t)))

(define (prepare-document t)
  (flatten-document (remove-creation-date t)))

(define (simplify-document* t)
  (cond ((nlist>0? t) t)
        ((not (func? t 'document)) (map simplify-document* t))
        (else
          `(document
                 ,@(map-in-order
                     (lambda (x)
                       (with y (simplify-document* x)
                         (if (func? y 'document 1) (cadr y) y)))
                     (cdr t))))))

(define (resolve-range ref curr*)
  (let* ((curr     (simplify-document* (cDr curr*)))
         (assocs   (cdadr (cAr   curr*)))
         (assocs   (map-in-order caddr assocs))
         (assocs   (filter (lambda (x) (!= '(document) (cadr x))) assocs))
         (rbody    (cdadr (caddr ref)))
         (cbody    (cdadr (caddr curr)))
         (diffs    (map (lambda (cpar)
                          (with l (map (lambda (rpar) (== rpar cpar)) rbody)
                            (with nor-l (nin? #t l)
                              (if nor-l (delete1! cpar rbody))
                              nor-l)))
                        cbody))
         (ranges   (map-in-order cdddr assocs))
         (ranges*  (map-in-order
                     (lambda (x p)
                       (if p (map string->number x) '())) ranges diffs))
         (ranges*  (filter nnull? ranges*)))
  ranges*))

(define (secured-latex-document->texmacs s as-pic keep-src)
  (let* ((range        `((0 ,(string-length s))))
         (reference    (cpp-latex-document->texmacs s as-pic keep-src range))
         (s-reference  (prepare-document (cDr (tree->stree reference))))
         (range        '())
         (s-old        '())
         (current      (cpp-latex-document->texmacs s as-pic keep-src range))
         (s-current*   (tree->stree current))
         (s-current    (prepare-document (cDr s-current*))))
    (while (and (!= s-reference s-current) (!= s-old s-current)
                (or (< (length range) 2) (!= (cAr range) (cADr range))))
           (set! range
             (append range (resolve-range s-reference s-current*)))
           (set! s-old s-current)
           (set! current
             (cpp-latex-document->texmacs s as-pic keep-src range))
           (set! s-current* (tree->stree current))
           (set! s-current (prepare-document (cDr s-current*))))
    (if (== s-old s-current)
      (display* "TeXmacs] LaTeX: the secure tracking is not garanteed"))
  current))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-define (convert latex test-tmtex) test-tmtex)
