
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-tools.scm
;; DESCRIPTION : Routines for expansion of macros and preamble construction
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-tools)
  (:use (convert latex latex-drd)
        (convert latex texout)))

(tm-define tmtex-cjk-document? #f)
(tm-define tmtex-use-catcodes? #f)
(tm-define tmtex-use-unicode? #f)
(tm-define tmtex-use-ascii? #f)
(tm-define tmtex-use-macros? #f)

(define latex-language "english")
(define latex-style "generic")
(define latex-packages '())
(define latex-extra-packages '())
(define latex-virtual-packages '())
(define latex-all-packages '())
(define latex-texmacs-style "generic")
(define latex-texmacs-packages '())
(define latex-dependencies '("generic"))

(define latex-packages-option (make-ahash-table))
(define latex-uses-table (make-ahash-table))
(define latex-catcode-table (make-ahash-table))
(define latex-macro-table (make-ahash-table))
(define latex-env-table (make-ahash-table))
(define latex-preamble-table (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting and testing global parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-set-language lan)
  (set! latex-language lan))

(tm-define (latex-set-style sty)
  (set! latex-style sty)
  (latex-set-dependencies))

(tm-define (latex-set-packages ps)
  (set! latex-packages ps)
  (latex-set-dependencies))

(tm-define (latex-set-extra ps)
  (set! latex-extra-packages ps)
  (latex-set-dependencies))

(tm-define (latex-add-extra p)
  (when (nin? p latex-extra-packages)
    (set! latex-extra-packages (cons p latex-extra-packages))
    (latex-set-dependencies)))

(tm-define (latex-set-virtual-packages ps)
  (set! latex-virtual-packages ps)
  (latex-set-dependencies))

(tm-define (latex-set-texmacs-style sty)
  (set! latex-texmacs-style sty))

(tm-define (latex-set-texmacs-packages l)
  (set! latex-texmacs-packages l))

(define (latex-set-dependencies)
  (set! latex-all-packages
        (list-remove-duplicates (append latex-packages
                                        latex-extra-packages
                                        latex-virtual-packages)))
  (set! latex-dependencies
        (latex-packages-dependencies (cons latex-style latex-all-packages))))

(tm-define (latex-has-style? sty)
  (== sty latex-style))

(tm-define (latex-has-package? p)
  (in? p latex-packages))

(tm-define (latex-has-texmacs-style? sty)
  (== sty latex-texmacs-style))

(tm-define (latex-has-texmacs-package? p)
  (in? p latex-texmacs-packages))

(tm-define (latex-depends? p)
  (in? p latex-dependencies))

(tm-define (latex-book-style?)
  (in? latex-style '("book" "svmono")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catcode generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-catcode-defs-char c)
  (let* ((s (string-convert (list->string (list c)) "Cork" "UTF-8"))
         (r (string-convert s "UTF-8" "LaTeX")))
    (if (and (!= r s) (!= (string c) "\n"))
      (ahash-set! latex-catcode-table (string c) r))))

(define (latex-catcode-defs-sub doc)
  (cond ((string? doc) (for-each latex-catcode-defs-char (string->list doc)))
        ((list? doc) (for-each latex-catcode-defs-sub doc))))

(define (latex-catcode-defs-char* c)
  (if (in? c '(#\< #\>))
    (ahash-set! latex-catcode-table (string c)
                (number->string (char->integer c)))))

(define (env? t x)
  (and (list>0? t) (func? (car t) '!begin) (list>1? (car t)) (== x (cadar t))))

(define (latex-is-math? t)
  (or (func? t '!math)
      (func? t '!eqn)
      (env? t "equation")
      (env? t "gather")
      (env? t "multline")
      (env? t "split")
      (env? t "equation*")
      (env? t "gather*")
      (env? t "multline*")
      (env? t "align")
      (env? t "flalign")
      (env? t "alignat")
      (env? t "align*")
      (env? t "flalign*")
      (env? t "alignat*")))

(define (latex-is-text? t)
  (func? t 'text))

(define (latex-is-verb? t)
  ;; TODO: consider also macros which expect verbatim args
  (or (func? t '!verb)      (func? t '!verbatim)
      (func? t '!verbatim*) (func? t 'tmverbatim)))

(define (latex-catcode-defs-sub* doc text?)
  (cond ((and text? (string? doc))
         (for-each latex-catcode-defs-char* (string->list doc)))
        ((and (list? doc) (latex-is-text? doc))
         (for-each (cut latex-catcode-defs-sub* <> #t) doc))
        ((and (list? doc) (or (latex-is-math? doc) (latex-is-verb? doc)))
         (for-each (cut latex-catcode-defs-sub* <> #f) doc))
        ((list? doc)
         (for-each (cut latex-catcode-defs-sub* <> text?) doc))))

(define (latex-catcode-def key im)
  (string-append "\\catcode`\\" key "=\\active \\def" key "{" im "}\n"))

(tm-define (latex-catcode-defs doc)
  (:synopsis "Return necessary catcode definitions for @doc")
  (string-append
    (if tmtex-use-catcodes?
      (begin
        (set! latex-catcode-table (make-ahash-table))
        (latex-catcode-defs-sub doc)
        (let* ((l1 (ahash-table->list latex-catcode-table))
               (l2 (list-sort l1 (lambda (x y) (string<=? (car x) (car y)))))
               (l3 (map (lambda (x) (latex-catcode-def (car x) (cdr x))) l2)))
          (apply string-append l3))) "")
    (begin
      (set! latex-catcode-table (make-ahash-table))
      (latex-catcode-defs-sub* doc #t)
      (let* ((l1 (ahash-table->list latex-catcode-table))
             (l2 (list-sort l1 (lambda (x y) (string<=? (car x) (car y)))))
             (keys (map car l2))
             (ims (map (lambda (x)
                         (string-append
                           "\n\\fontencoding{T1}\\selectfont\\symbol{"
                           (cdr x)
                           "}\\fontencoding{\\encodingdefault}"))
                       l2))
             (l3 (map latex-catcode-def keys ims)))
        (apply string-append l3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for reading the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (env-begin? x)
  (or (func? x '!begin) (func? x '!begin*)))

(define (latex-texmacs-arity x)
  (if (env-begin? x)
      (latex-texmacs-arity
       (string->symbol (string-append "begin-" (tex-env-name (cadr x)))))
      (logic-ref latex-texmacs-arity% x)))

(define (latex-needs? x)
  (if (env-begin? x)
      (latex-needs?
       (string->symbol (string-append "begin-" (tex-env-name (cadr x)))))
      (logic-ref latex-needs% x)))

(define (latex-texmacs-option? x)
  (if (env-begin? x)
      (latex-texmacs-option?
       (string->symbol (string-append "begin-" (tex-env-name (cadr x)))))
      (logic-ref latex-texmacs-option% x)))

(define (latex-texmacs-macro-body x)
  (smart-ref latex-texmacs-macro x))

(define (latex-texmacs-environment-body x)
  (smart-ref latex-texmacs-environment (tex-env-name x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro and environment expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-substitute t args)
  (cond ((number? t) (list-ref args t))
        ((== t '---) (car args))
        ((func? t '!recurse 1)
         (latex-expand-macros (latex-substitute (cadr t) args)))
        ((func? t '!translate 1)
         (translate-from-to (cadr t) "english" latex-language))
        ((list? t) (map (cut latex-substitute <> args) t))
        (else t)))

(tm-define (latex-expand-macros t)
  (:synopsis "Expand all TeXmacs macros occurring in @t")
  (if (npair? t) t
      (let* ((head  (car t))
             (tail  (map latex-expand-macros (cdr t)))
             (body  (latex-texmacs-macro-body head))
             (arity (and body (latex-texmacs-arity head)))
             (env   (and (env-begin? head)
                         (latex-texmacs-environment-body (cadr head))))
             (envar (and env (latex-texmacs-arity head))))
        (cond ((and body (== (length tail) arity))
               ;;(latex-substitute body t)
               (latex-substitute body (cons head tail)))
              ((and env (== (length tail) 1) (== (length (cddr head)) envar))
               ;;(latex-substitute env (append (cdr t) (cddr head)))
               (latex-substitute env (append tail (cddr head))))
              (else (cons head tail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute macro and environment definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-expand-def t protect?)
  (cond ((and protect? (number? t)) (set! t `(!group ,t)) (set! protect? #f))
        ((and (not protect?) (func? t '!option) (set! protect? #t))))
  (cond ((== t '---) "#-#-#")
        ((number? t) (string-append "#" (number->string t)))
        ((func? t '!recurse 1) (latex-expand-def (cadr t) protect?))
        ((func? t '!translate 1)
         (translate-from-to (cadr t) "english" latex-language))
        ((list? t) (map (cut latex-expand-def <> protect?) t))
        (else t)))

;; TODO: to be rewritten with better factorisation
(define (latex-macro-defs-sub t)
  (when (pair? t)
    (if (and (or (func? t 'newcommand) (func? t 'renewcommand))
             (> (length t) 2))
      (for-each latex-macro-defs-sub (cddr t))
      (for-each latex-macro-defs-sub (cdr t)))
    (let* ((body   (and (not (latex-needs? (car t)))
                        (latex-texmacs-macro-body (car t))))
           (arity  (and body (latex-texmacs-arity (car t))))
           (option (and body (latex-texmacs-option? (car t))))
           (args   (if option (filter (lambda (x)
                                        (not (and (list? x)
                                                  (== (car x) '!option))))
                                      (cdr t))
                     (cdr t))))
      (when (and body (== (length args) arity))
        (if option (set! arity (+ 1 arity)))
        (ahash-set! latex-macro-table (car t)
                    (list arity (latex-expand-def body #f)))
        (latex-macro-defs-sub body)))
    (let* ((body  (and (env-begin? (car t))
                       (not (latex-needs? (car t)))
                       (latex-texmacs-environment-body (cadar t))))
           (arity (and body (latex-texmacs-arity (car t))))
           (option (and body (latex-texmacs-option? (car t))))
           (args   (and body
                        (if option (filter (lambda (x)
                                             (not (and (list? x)
                                                       (== (car x) '!option))))
                                           (car t))
                            (car t)))))
      (when (and body (== (length args) (+ arity 2)))
        (if option (set! arity (+ 1 arity)))
        (ahash-set! latex-env-table (cadar t)
                    (list arity (latex-expand-def body #f)))
        (latex-macro-defs-sub body)))
    (with body (or (and (not (latex-needs? (car t)))
                        (smart-ref latex-texmacs-preamble (car t)))
                   (and (env-begin? (car t))
                        (not (latex-needs? (car t)))
                        (smart-ref latex-texmacs-env-preamble (cadar t))))
      (when body
        (ahash-set! latex-preamble-table
                    (if (env-begin? (car t)) (cadar t) (car t)) body)
        (latex-macro-defs-sub body)))))

(define (latex<=? x y)
  (if (symbol? x) (set! x (symbol->string x)))
  (if (symbol? y) (set! y (symbol->string y)))
  (if (env-begin? x) (set! x (cadr x)))
  (if (env-begin? y) (set! y (cadr y)))
  (string<=? x y))

(tm-define (latex-macro-defs t)
  (:synopsis "Return necessary macro and environment definitions for @doc")
  (set! latex-macro-table (make-ahash-table))
  (set! latex-env-table (make-ahash-table))
  (set! latex-preamble-table (make-ahash-table))
  (latex-macro-defs-sub t)
  (let* ((c1 (ahash-table->list latex-macro-table))
         (c2 (list-sort c1 (lambda (x y) (latex<=? (car x) (car y)))))
         (c3 (map (cut cons '!newcommand <>) c2))
         (e1 (ahash-table->list latex-env-table))
         (e2 (list-sort e1 (lambda (x y) (latex<=? (car x) (car y)))))
         (e3 (map (cut cons '!newenvironment <>) e2))
         (p1 (ahash-table->list latex-preamble-table))
         (p2 (list-sort p1 (lambda (x y) (latex<=? (car x) (car y)))))
         (p3 (map cdr (map (cut latex-expand-def <> #f) p2))))
    (cons '!append (append c3 e3 p3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization of TeXmacs preambles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-macro-def name arity body)
  (with option ""
    (if (and (list>1? body) (list? (car body)) (== (caar body) '!option))
      (begin
        (set! option (serialize-latex (latex-expand-def (cadar body) #f)))
        (set! option (string-append "[" option "]"))
        (set! body (cadr body))))
    (set! body (serialize-latex (latex-expand-def body #f)))
    (set! body (string-replace body "\n\n" "*/!!/*"))
    (set! body (string-replace body "\n" " "))
    (set! body (string-replace body "*/!!/*" "\n\n"))
    (set! arity (if (= arity 0) ""
                  (string-append "[" (number->string arity) "]")))
    (string-append "\\newcommand{\\" (symbol->string name) "}"
                   arity option "{" body "}\n")))

(define (latex-env-def name arity body)
  (with option ""
    (if (and (list>1? body) (list? (car body)) (== (caar body) '!option))
      (begin
        (set! option (serialize-latex (latex-expand-def (cadar body) #f)))
        (set! option (string-append "[" option "]"))
        (set! body (cadr body))))
    (set! body (serialize-latex (latex-expand-def body #f)))
    (set! body (string-replace body "%\n#-#-#" "#-#-#"))
    (set! body (string-replace body "%\n  #-#-#" "#-#-#"))
    (set! body (string-replace body "\n\n" "*/!!/*"))
    (set! body (string-replace body "\n  " " "))
    (set! body (string-replace body "\n" " "))
    (set! body (string-replace body "   #-#-# " "}{"))
    (set! body (string-replace body "#-#-# " "}{"))
    (set! body (string-replace body "#-#-#" "}{"))
    (set! body (string-replace body "*/!!/*" "\n\n"))
    (set! arity (if (= arity 0) ""
                  (string-append "[" (number->string arity) "]")))
    (string-append "\\newenvironment{" (tex-env-name name) "}"
                   arity option "{" body "}\n")))

(tm-define (latex-serialize-preamble t)
  (:synopsis "Serialize a LaTeX preamble @t")
  (cond ((string? t) t)
        ((func? t '!append)
         (apply string-append (map latex-serialize-preamble (cdr t))))
        ((func? t '!newcommand 3) (apply latex-macro-def (cdr t)))
        ((func? t '!newenvironment 3) (apply latex-env-def (cdr t)))
        (else (serialize-latex t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package dependencies management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-package-direct-dependencies p)
  (logic-ref-list latex-depends% p))

(define (insert-dependencies l p)
  (if (null? p) l
      (if (in? (car p) l)
          (insert-dependencies l (cdr p))
          (with deps (latex-package-direct-dependencies (car p))
            (insert-dependencies (append l (list (car p)))
                                 (append deps (cdr p)))))))

(tm-define (latex-packages-dependencies ps)
  (:synopsis "Determine all dependencies of packages @ps")
  (insert-dependencies (list) ps))

(define (non-redundant-package? p among)
  (with c (latex-packages-dependencies (list-difference among (list p)))
    (not (in? p c))))

(tm-define (latex-packages-simplify ps)
  (:synopsis "Remove all implied packages in package list @ps")
  (list-filter ps (lambda (p) (non-redundant-package? p ps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute usepackage command for a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-command-uses s)
  (with packlist (logic-ref-list latex-needs% s)
    (for-each (cut ahash-set! latex-uses-table <> #t) packlist)))

(define (latex-use-which-package l)
  (when (and (list? l) (nnull? l))
    (let ((x (car l)))
      (when (symbol? x)
        (with s (symbol->string x)
          (cond ((string-starts? s "left\\")
                 (latex-command-uses (string->symbol (string-drop s 5))))
                ((string-starts? s "right\\")
                 (latex-command-uses (string->symbol (string-drop s 6))))
                (else (latex-command-uses x)))))
      (if (and (list? x) (>= (length l) 2) (== (car x) '!begin))
          (latex-command-uses
           (string->symbol (string-append "begin-" (cadr x)))))
      (if (match? x '(!begin "enumerate" (!option :%1)))
          (ahash-set! latex-uses-table "enumerate" #t))
      (for-each latex-use-which-package (cdr l)))))

(define (latex-use-package-compare l r)
  (let* ((tl (logic-ref latex-package-priority% l))
         (tr (logic-ref latex-package-priority% r))
         (vl (if tl tl 999999))
         (vr (if tr tr 999999)))
    (< vl vr)))

(define (filter-packages l)
  (filter (lambda (x) (nin? x (tmtex-provided-packages))) l))

(define (filter-packages* l)
  (filter (lambda (x) (nin? (cAr x) (tmtex-provided-packages))) l))

(define (make-use-package l)
  (with po (ahash-ref latex-packages-option (cAr l))
    (let* ((optl (if (not po) (cDr l) (append (cDr l) po)))
           (opt  (apply string-append (list-intersperse optl ",")))
           (sty  (cAr l)))
      (with opts (if (== opt "") "" (string-append "["  opt "]"))
        (string-append "\\usepackage" opts "{" sty "}\n")))))

(tm-define (latex-ifacconf-style?)
  (== tmtex-style "ifacconf"))

(tm-define (latex-as-use-package l1)
  (let* ((l2  (sort l1 latex-use-package-compare))
         (l3  (filter
                (lambda (x)
                  (and (string? x)
                       (not (ahash-ref latex-packages-option x))))
                l2))
         (l3* (map (lambda (x)
                     (map force-string x))
                   (filter
                     list>0?
                     (map
                       (lambda (x)
                         (if (ahash-ref latex-packages-option x) (list x) x))
                       l2))))
         (l4  (filter-packages  l3))
         (l4* (filter-packages* l3*))
         (l5  (list-intersperse l4 ","))
         (s   (apply string-append l5))
         (s*  (apply string-append (map make-use-package l4*))))
    (if (== s "") s* (string-append "\\usepackage{" s "}\n" s*))))

(tm-define (latex-use-package-command doc)
  (:synopsis "Return the usepackage command for @doc")
  (set! latex-uses-table (make-ahash-table))
  (latex-use-which-package doc)
  (for (p (ahash-table->list latex-packages-option))
    (ahash-set! latex-uses-table (car p) #t))
  (let* ((l1 latex-all-packages)
         (s1 (latex-as-use-package (list-difference l1 '("amsthm"))))
         (l2 (map car (ahash-table->list latex-uses-table)))
         (s2 (latex-as-use-package (list-difference l2 l1))))
    (string-append s1 s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page size settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-preamble-page-type init)
  (let* ((l0 (ahash-table->list init))
         (l1 (map car l0))
         (l2 (map cdr l0))
         (l3 (map (cut logic-ref latex-paper-opts% <>) l1))
         (l4 (map (lambda (key val)
                    (cond ((not val) #f)
                          ((== key "page-type")
                           (or (logic-ref latex-paper-type% val) '()))
                          ((== key "page-orientation") val)
                          ((and (string? key) (!= val "auto"))
                           (string-append key "=" (tmtex-decode-length val)))
                          (else #f))) l3 l2))
         (l5 (filter string? l4))
         (page-opts (list-intersperse l5 ",")))
    (if (nnull? page-opts)
      `(!append (geometry (!concat ,@page-opts)) "\n") "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (html-color->latex-xcolor s)
  "Take an hexa html color string and return an hex triplet string"
  (upcase-all
   (cond ((string-starts? s "#") (html-color->latex-xcolor (string-tail s 1)))
         ((== 3 (string-length s))
          (let ((r (substring s 0 1))
                (g (substring s 1 2))
                (b (substring s 2 3)))
            (string-append r r g g b b)))
         ((== 4 (string-length s)) (html-color->latex-xcolor (string-take s 3)))
         ((== 6 (string-length s)) s)
         ((== 8 (string-length s)) (string-take s 6))
         (else s))))

(define (latex-colors-defs colors)
  (apply string-append
         (map (lambda (x)
                (string-append
                  "\\definecolor{" (string-replace x " " "") "}{HTML}{"
                  (html-color->latex-xcolor (get-hex-color x)) "}\n"))
              colors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the preamble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-make-option l)
  (string-append "[" (apply string-append (list-intersperse l ",")) "]"))

(define (set-packages-option pack opts colors)
  (cond ((nnull? opts)
         (ahash-set! latex-packages-option pack opts))
        ((nnull? colors)
         (ahash-set! latex-packages-option pack (list "")))))

(tm-define (latex-extra-preamble) "")

(tm-define (latex-preamble text style lan init colors colormaps)
  (:synopsis "Compute preamble for @text")
  (with-global tmtex-style (if (list? style) (cAr style) style)
    (set! latex-packages-option (make-ahash-table))
    (set-packages-option "xcolor" colormaps colors)
    (let* ((Page         (latex-preamble-page-type init))
           (Macro        (latex-macro-defs text))
           (Colors       (latex-colors-defs colors))
           (Text         (list '!tuple Page Macro Colors text))
           (pre-page     (latex-serialize-preamble Page))
           (pre-macro    (latex-serialize-preamble Macro))
           (pre-colors   (latex-serialize-preamble Colors))
           (pre-catcode  (latex-catcode-defs Text))
           (pre-uses     (latex-use-package-command Text))
           (pre-extra    (latex-extra-preamble)))
      (values
        (cond ((and (in? "amsthm" latex-all-packages)
                    (== style "amsart")) "[amsthm]")
              ((list? style) (latex-make-option (cDr style)))
              (else ""))
        (string-append pre-uses pre-extra)
        (string-append pre-page)
        (string-append pre-catcode pre-macro pre-colors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean-up the produced LaTeX for use with MathJax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-mathjax-text l arg)
  (with x (latex-mathjax-pre arg)
    (cond ((or (npair? x) (nlist? x)) `(,l ,x))
          ((func? x 'tmtextsf 1) (latex-mathjax-text 'textsf (cadr x)))
          ((func? x 'tmtexttt 1) (latex-mathjax-text 'texttt (cadr x)))
          ((func? x 'tmtextit 1) (latex-mathjax-text 'textit (cadr x)))
          ((func? x 'tmtextbf 1) (latex-mathjax-text 'textbf (cadr x)))
          ((func? x 'tmtextrm 1) (latex-mathjax-text l (cadr x)))
          ((func? x 'tmtextup 1) (latex-mathjax-text l (cadr x)))
          (else `(,l ,x)))))

(tm-define (latex-mathjax-pre x)
  (:synopsis "Produce cleaner LaTeX for @x for use with MathJax, pass 1")
  (cond ((or (npair? x) (nlist? x)) x)
        ((func? x 'text 1)
         (latex-mathjax-text 'text (cadr x)))
        ((func? x 'dotminus 0) `(dot "-"))
        ((func? x 'dotpm 0) `(dot (pm)))
        ((func? x 'dotmp 0) `(dot (mp)))
        ((func? x 'dotamalg 0) `(dot (amalg)))
        ((func? x 'dotplus 0) `(dot "+"))
        ((func? x 'dottimes 0) `(dot (times)))
        ((func? x 'dotast 0) `(dot (ast)))
        ((func? x 'dag) `(dagger))
        ((and (func? x 'color 2) (func? (cadr x) '!option 1))
         ;; NOTE : MathJax has broken color support, so ignore certain colors
         ;; FIXME: this hack may have to be suppressed when MathJax improves
         "")
        (else (cons (car x) (map latex-mathjax-pre (cdr x))))))

(tm-define (latex-mathjax x)
  (:synopsis "Produce cleaner LaTeX for @x for use with MathJax, pass 2")
  (cond ((or (npair? x) (nlist? x)) x)
        ((func? x 'ensuremath 1) (latex-mathjax (cadr x)))
        ((func? x 'hspace* 1) `(hspace ,(latex-mathjax (cadr x))))
        ((func? x 'mathbbm 1) `(mathbb ,(latex-mathjax (cadr x))))
        ((func? x 'fill 0) "3cm")
        ((func? x 'newcommand) "")
        ((func? x 'custombinding) "")
        ((func? x 'nobreak) "")
        ((func? x 'label) "")
        (else (cons (car x) (map latex-mathjax (cdr x))))))
