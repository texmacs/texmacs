;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmarkdown.scm
;; DESCRIPTION : markdown-stree to markdown-document or markdown-snippet
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown markdownout)
  (:use (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Global" state for document serialization and config options
;; Usage is wrapped within a "with-global" in serialize-markdown-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define footnote-nr 0)
(define label-nr 0)  ; global counter for labels (incl. labeled equations)
(define equation-nr 0)
(define environment-nr 0)  ; global counter for numbered environments
(define num-line-breaks 2)
(define paragraph-width #f)
(define paper-authors '())
(define post-tags '())
(define post-author "")
(define doc-title "")
(define postlude "")
(define labels '())
(define indent "")
(define (first-indent) indent)
(define file? #f)

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:hugo-extensions") "on"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: does this make sense? We only convert if exporting to file.
; The idea is that ""Copy to markdown" might already perform some internal
; conversion before sending us the stree, because weird chars appear.
; However if we don't do any conversion here, the copied text is still wrong
(define (tm-encoding->md-encoding x)
  (if file? (string-convert x "Cork" "UTF-8") x))

(define (md-encoding->tm-encoding x)
  (if file? (string-convert x "UTF-8" "Cork") x))

(define (list->csv l)
  (string-join (map (cut string-append "\"" <> "\"") l) ", "))

(define (indent-increment s)
  (string-append indent s))

(define (author-add x)
  (set! paper-authors (append paper-authors (cdr x)))
  "")

(define (md-doc-running-author x)
  (set! post-author (cadr x))
  "")

(define (indent-decrement n)
  (lambda () 
    (if (> (string-length indent) n)
        (string-drop-right indent n)
        "")))

(define (prelude)
  "Output Hugo frontmatter"
  (if (not (hugo-extensions?)) ""
      (let ((paper-authors* (list->csv paper-authors))
            (post-tags* (list->csv post-tags))
            (date (strftime "%Y-%m-%d"(localtime (current-time)))))
        (string-append "---\n\n"
                       "title: \"" doc-title "\"\n"
                       "author: \"" post-author "\"\n"
                       "authors: [\"" post-author "\"]\n"
                       "date: \"" date "\"\n"
                       "tags: [" post-tags* "]\n"
                       "paper_authors: [" paper-authors* "]\n"
                       "paper_key: \"\"\n\n"
                       "---\n\n"))))

(define (postlude-add x)
  (cond ((list? x) 
         (set! postlude 
               (string-concatenate `(,postlude
                                     "\n[^" ,(number->string footnote-nr) "]: "
                                     ,@(map serialize-markdown x)))))
        ((string? x)
         (set! postlude (string-append postlude "\n" x)))
        (else 
          (display* "postlude-add: bogus input " x "\n")
          (noop))))

; There are probably a dozen functions in TeXmacs doing the 
; very same thing as these two...
(define (replace-fun-sub where what? by)
  (if (npair? where) (if (what? where) (by where) where)
      (cons (if (what? (car where)) (by (car where))
                (replace-fun-sub (car where) what? by))
            (replace-fun-sub (cdr where) what? by))))

; This looks familiar... :/
(define (replace-fun where what by)
 (cond ((not (procedure? what))
        (replace-fun where (cut == <> what) by))
       ((not (procedure? by))
        (replace-fun where what (lambda (x) by)))
       (else (replace-fun-sub where what by))))

(define (replace-fun-list where rules)
  (if (and (list>0? rules) (pair? (car rules)))
      (replace-fun (replace-fun-list where (cdr rules))
                   (caar rules) (cdar rules))
      where))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown to string serializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep x)
  (cons (car x) (map serialize-markdown (cdr x))))

(define (skip x)
  (string-concatenate (map serialize-markdown (cdr x))))

(define (md-string s)
  (tm-encoding->md-encoding s))

(define (adjust-width s cols prefix first-prefix)
  (if (not paragraph-width)  ; set paragraph-width to #f to disable adjustment
      (md-string (string-append prefix s))
      (let* ((l (map md-string (string-split s #\ ))) ;split words
             (c (string-length prefix))
             (line-len 0)
             (proc (lambda (w acc)
                     (set! line-len (+ line-len (string-length w) 1))
                     (if (> line-len cols)
                         (begin
                           (set! line-len (+ c (string-length w)))
                           (string-append acc "\n" prefix w " "))
                         (string-append acc w " ")))))
        (string-trim-right (list-fold proc first-prefix l)))))

(define (md-must-adjust? t)
  (and (list>1? t)
       (in? (car t)
            '(strong em tt strike math concat cite cite-detail 
                     eqref reference figure hlink))))

(define (md-paragraph p)
  (cond ((string? p)
         ;; FIXME: arguments of Hugo shortcodes shouldn't be split
         (adjust-width p paragraph-width indent (first-indent)))
        ((md-must-adjust? p)
         (adjust-width (serialize-markdown p) paragraph-width indent
                       (first-indent)))
        (else ;; do not convert to prevent nested conversions
          (serialize-markdown p))))

(define (md-document x)
  (string-concatenate
   (list-intersperse (map md-paragraph (cdr x))
                     (string-concatenate (make-list num-line-breaks "\n")))))

(define (md-concat x)
  (string-concatenate (map serialize-markdown (cdr x))))

(define (line-breaks-after s)
  (string-concatenate `(,s ,@(make-list num-line-breaks "\n"))))

(define (md-header n)
  (lambda (x)
    (with-global num-line-breaks 0
      (with res (string-concatenate
                 `(,@(make-list n "#")
                   " "
                   ,@(map serialize-markdown (cdr x))))
            (if (> n 4)  ; Special handling of TeXmacs <paragraph>
                (string-append res " ")
                res)))))

(define (math->latex t)
 "Converts the TeXmacs tree @t into internal LaTeX representation"
 (with options '(("texmacs->latex:replace-style" . "on")
                 ("texmacs->latex:expand-macros" . "on")
                 ("texmacs->latex:expand-user-macros" . "off")
                 ("texmacs->latex:indirect-bib" . "off")
                 ("texmacs->latex:encoding" . "utf8")
                 ("texmacs->latex:use-macros" . "off"))
 (texmacs->latex t options)))

(define (md-environment x)
  (set! environment-nr (+ 1 environment-nr))
  (set! label-nr (+ 1 label-nr))
  (let* ((txt (translate (string-capitalize (symbol->string (car x)))))
         (nr (number->string environment-nr))
         (tag `(strong ,(string-append txt " " nr ":")))
         (content (cdadr x)))
    (serialize-markdown 
     `(document (concat ,tag " " ,(car content)) ,@(cdr content)))))

(define (md-environment* x)
  (let* ((s (string-drop-right (symbol->string (car x)) 1))
         (txt (translate (string-capitalize s)))
         (tag `(strong ,(string-append txt ":")))
         (content (cdadr x)))
    (serialize-markdown 
     `(document (concat ,tag " " ,(car content)) ,@(cdr content)))))

(define (md-dueto x)
  (serialize-markdown
   `(concat " " (em (concat "(" ,(cadr x) ")")) " ")))

(define (md-fix-math-row t)
  "Append backslashes to last item in a !row"
  (if (and (func? t '!row) (list>1? t))
      (with cols (cdr t)
        `(!row ,@(cDr cols) (!concat ,(cAr cols) "\\\\\\\\")))
      t))

(define (md-fix-math-table t)
  "Append extra backslashes at the end of !rows in LaTeX tables"
  (if (not (list>1? (cdr t))) t  ; Nothing to do with only one row
      (let* ((rows (cdr t))
             (last-row (cAr rows))
             (first-rows (cDr rows)))
        `(!table ,@(map md-fix-math-row first-rows) ,last-row))))

(define (md-math* t)
  (replace-fun-list t
   `((mathbbm . mathbb)
     ((_) . "\\_")
     (({) . (lbrace))
     ((}) . (rbrace))
     ((left\{) . (left\lbrace))
     ((right\}) . (right\rbrace))
     (,(cut func? <> '!table) . ,md-fix-math-table)
     (,(cut func? <> 'ensuremath) . ,cadr)
     (,(cut func? <> '!sub) . 
       ,(lambda (x) (cons "\\_" (cdr x))))
     (,(cut func? <> 'label) .   ; append tags to labels
       ,(lambda (x)
          (set! equation-nr (+ 1 equation-nr))
          (with label-name (number->string equation-nr)
            (ahash-set! labels (cadr x) label-name)
            (list '!concat x `(tag ,label-name))))))))

(define (md-math x . leave-newlines?)
 "Takes an stree @x, and returns a valid MathJax-compatible LaTeX string"
 (with ltx (math->latex x)
   (if (null? leave-newlines?)
       (string-replace (serialize-latex (md-math* ltx)) "\n" " ")
       (serialize-latex (md-math* ltx)))))

(define (md-equation x)
  ;; HACK
  (let*  ((s (md-math x #t))
          (s1 (string-replace s "\\[" "\\\\["))
          (s2 (string-replace s1 "\\]" "\\\\]"))
          (lines (string-split s2 #\newline)))
    (with-global num-line-breaks 1
      (serialize-markdown
       `(document ,@lines)))))

(define (md-eqref x)
  (let* ((label (cadr x))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-name (ahash-ref labels label err-msg)))
    (string-append "(" label-name ")")))

(define (md-label x)
  (let ((label-name (number->string label-nr))
        (label (cadr x)))
    (if (not (ahash-ref labels label))
        (begin (ahash-set! labels label label-name) "")
        (begin (string-append "duplicate label: '" label "'")))))

(define (md-reference x)
  (let* ((label (cadr x))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-name (ahash-ref labels label err-msg)))
    label-name))

(define (md-item? x)
  (and (list>0? x) (func? x 'concat) (== (cadr x) '(item))))

(define (md-list x)
  (let* ((c (cond ((== (car x) 'itemize) "* ")
                  ((== (car x) 'enumerate) "1. ")
                  ((== (car x) 'enumerate-alpha) "a. ")
                  (else "* ")))
         (cs (string-concatenate (make-list (string-length c) " ")))
         (transform
          (lambda (a)
            (if (md-item? a) `(concat ,c ,@(cddr a)) a))))
    (with doc (cAr x)
      (with-global num-line-breaks 1
        (with-global indent (indent-increment cs)
          (with-global first-indent (indent-decrement (string-length c))
            (serialize-markdown `(document ,@(map transform (cdr doc))))))))))

(define (md-quotation x)
  (with-global num-line-breaks 1
    (with-global indent (indent-increment "> ")
      (serialize-markdown (cAr x)))))

(define (style-text style)
 (cond ((== style 'strong) "**")
       ((== style 'em) "*")
       ((== style 'tt) (md-encoding->tm-encoding "`"))
       ((== style 'strike) "~~")
       (else "")))

(define (md-style x)
  (with st (style-text (car x))
    (string-concatenate 
     `(,st ,@(map serialize-markdown (cdr x)) ,st " "))))

(define (md-cite x)
  (if (not (hugo-extensions?)) ""
      (string-concatenate
       (list-intersperse
        (map (cut string-append "{{< cite " <> " >}}") (cdr x))
        ", "))))

(define (md-cite-detail x)
  (with detail (cAr x)
      (string-append (md-cite (cDr x)) " (" detail ")")))

(define (md-hlink x)
  (with payload (cdr x)
    (string-append "[" (serialize-markdown (car payload)) "]"
                   "(" (cadr payload) ")")))    

(define (md-figure x)
  "Hugo {{< figure >}} shortcode"
  (if (hugo-extensions?)
      (with payload (cdr x)
        (with-global num-line-breaks 0
          (string-concatenate 
           `("{{< figure src=\"" ,(car payload) 
             "\" title=\"" ,@(map serialize-markdown (cdr payload)) "\" >}}"))))
      ""))

(define (md-footnote x)
  ; Input: (footnote (document [stuff here]))
  (set! footnote-nr (+ 1 footnote-nr))
  (with-global num-line-breaks 0
    (with-global indent ""
      (with-global paragraph-width #f
        (postlude-add (cdr x))
        (string-append "[^" (number->string footnote-nr) "]")))))

(define (md-doc-title x)
  (set! doc-title (md-string (serialize-markdown (cdr x))))
  (if (hugo-extensions?) ""
      ((md-header 1) (cdr x))))

(define (md-block x)
  (with-global num-line-breaks 1
    (with syntax (tm-ref x 0)
      (string-concatenate 
       `("```" ,syntax "\n" ,@(map serialize-markdown (cddr x)) "```\n")))))

(define (md-hugo-tags x)
  (if (hugo-extensions?)
      (begin (set! post-tags (cdr x)) "")
      (string-append "Tags: " (list->csv (cdr x)))))

(define (md-hugo-shortcode x)
  (if (hugo-extensions?)
      (string-concatenate 
       `("{{< " ,(cadr x) " " ,@(list-intersperse (cddr x) " ") " >}}"))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define serialize-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l)) 
     (list (list 'strong md-style)
           (list 'em md-style)
           (list 'tt md-style)
           (list 'strike md-style)
           (list 'block md-block)
           (list 'document md-document)
           (list 'quotation md-quotation)
           (list 'definition md-environment)
           (list 'definition* md-environment*)           
           (list 'conjecture md-environment)
           (list 'conjecture* md-environment*)           
           (list 'question md-environment)
           (list 'question* md-environment*)           
           (list 'algorithm md-environment)
           (list 'algorithm* md-environment*)           
           (list 'problem md-environment)
           (list 'problem* md-environment*)           
           (list 'theorem md-environment)
           (list 'theorem* md-environment*)           
           (list 'proposition md-environment)
           (list 'proposition* md-environment*)           
           (list 'corollary md-environment)
           (list 'corollary* md-environment*)           
           (list 'lemma md-environment)
           (list 'lemma* md-environment*)           
           (list 'proof md-environment)
           (list 'dueto md-dueto)
           (list 'math md-math)
           (list 'equation md-equation)
           (list 'equation* md-equation)
           (list 'concat md-concat)
           (list 'itemize md-list)
           (list 'enumerate md-list)
           (list 'enumerate-alpha md-list)
           (list 'h1 (md-header 1))
           (list 'h2 (md-header 2))
           (list 'h3 (md-header 3))
           (list 'h4 (md-header 4))
           (list 'doc-title md-doc-title)
           (list 'doc-running-author md-doc-running-author)
           (list 'author-name author-add)
           (list 'cite md-cite)
           (list 'cite-detail md-cite-detail)
           (list 'eqref md-eqref)
           (list 'label md-label)
           (list 'reference md-reference)
           (list 'footnote md-footnote)
           (list 'figure md-figure)
           (list 'hlink md-hlink)
           (list 'tags md-hugo-tags)  ; Hugo extension
           (list 'hugo md-hugo-shortcode)  ; Hugo extension
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (serialize-markdown x)
  (cond ((null? x) "")
        ((string? x) x)
        ((symbol? x) 
         (display* "Ignoring symbol " x "\n")
         "")
        ((symbol? (car x))
         (with fun 
              (ahash-ref serialize-hash (car x))
            (if (!= fun #f)
                (fun x)
                (begin
                  (display* "Serialize skipped " (car x) "\n")
                  (skip x)))))
        (else
         (string-concatenate
                (map serialize-markdown x)))))

(tm-define (serialize-markdown-document x)
  (with-global file? #t
    (with-global num-line-breaks 2
      (with-global indent ""
        (with-global labels (make-ahash-table)
          (with-global footnote-nr 0
            (with-global label-nr 0
              (with-global environment-nr 0                             
                (with-global equation-nr 0
                  (with-global paper-authors '()
                    (with-global post-tags '()
                      (with-global post-author ""
                        (with-global postlude ""
                          (with-global paragraph-width
                              (string->number (get-preference "texmacs->markdown:paragraph-width"))
                            (with body (serialize-markdown x)
                              (string-append (prelude)
                                             body
                                             postlude))))))))))))))))
