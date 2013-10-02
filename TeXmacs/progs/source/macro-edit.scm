
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-edit.scm
;; DESCRIPTION : editing macros
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-edit)
  (:use (generic document-part)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the definition of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first-match l pred?)
  (cond ((null? l) #f)
        ((pred? (car l)) (pred? (car l)))
        (else (first-match (cdr l) pred?))))

(define (get-definition** l t)
  (cond ((and (tree-func? t 'assign 2)
              (tree-atomic? (tree-ref t 0))
              (== (tree->string (tree-ref t 0)) l)) t)
        ((tree-atomic? t) #f)
        ((tree-in? t '(document concat surround with))
         (first-match (reverse (tree-children t))
                      (cut get-definition** l <>)))
        (else #f)))

(define (get-definition* l t)
  (cond ((tm-func? t 'hide-preamble 1) (get-definition** l (tree-ref t 0)))
        ((tm-func? t 'show-preamble 1) (get-definition** l (tree-ref t 0)))
        ((tree-atomic? t) #f)
        ((tree-in? t '(document concat surround with))
         (first-match (tree-children t) (cut get-definition* l <>)))
        (else #f)))

(tm-define (get-definition l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (get-definition* l (buffer-tree))
      (tree 'assign l (get-init-tree l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering of edit-macro tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-edit-macro a)
  (:secure #t)
  (let* ((c (tree-children a))
         (name (car c))
         (args (cDr (cdr c)))
         (args* (map (lambda (x) `(src-arg ,x)) args))
         (body (cAr c)))
  `(document
     (concat
       (inline-tag ,name ,@args*)
       " "
       (math "<assign>"))
     ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget for editing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macro-retrieve* u)
  (and-with t (buffer-get-body u)
    (if (tm-is? t 'document) (set! t (tm-ref t 0)))
    (and (tm-is? t 'edit-macro) t)))

(define (macro-retrieve u)
  (and-with t (macro-retrieve* u)
    (cond ((tm-is? (tree-ref t :last) 'inactive*)
           `(edit-macro ,@(cDr (tm-children t)) ,(tree-ref t :last 0)))
          (else t))))

(define (set-macro-mode u mode)
  (and-with t (macro-retrieve u)
    (with t* (macro-retrieve* u)
      (cond ((== mode "Source")
             (tree-set t* :last `(inactive* ,(cAr (tm-children t)))))
            (else
             (tree-set t* :last (cAr (tm-children t))))))))

(define (buffer-has-preamble? buf)
  (tree-in? (tree-ref buf 0)
            '(show-preamble hide-preamble)))

(define (preamble-insert pre ass)
  (with m (list-find (reverse (tree-children pre))
                     (lambda (x)
                       (and (tree-is? x 'assign)
                            (tm-equal? (tm-ref x 0) (tm-ref ass 0)))))
    (if m
        (tree-set m ass)
        (tree-insert pre (tree-arity pre) (list ass)))))

(define (macro-apply b u old)
  (and-with t (macro-retrieve u)
    (let* ((new `(macro ,@(cdr (tm-children t))))
           (ass `(assign ,(tm-ref t 0) ,new))
           (buf (buffer-get-body b)))
      (if (tree->path old)
          (tree-set old 1 new)
          (begin
            (when (not (buffer-has-preamble? buf))
              (tree-insert! buf 0 '((hide-preamble (document "")))))
            (when (buffer-has-preamble? buf)
              (with pre (tree-ref buf 0 0)
                (preamble-insert pre ass))))))))

(tm-widget ((macro-editor b u packs l def) quit)
  (padded
    (resize "600px" "300px"
      (texmacs-input
        `(document
           (edit-macro ,l ,@(tm-children (tm-ref def 1))))
        `(style (tuple ,@packs))
        u))
    ===
    (hlist
      (enum (set-macro-mode u answer)
            '("Text" "Source")
            "Text" "6em")
      >>
      (explicit-buttons
        ("Apply" (macro-apply b u def))
        //
        ("Ok" (begin (macro-apply b u def) (quit)))))))

(tm-define (open-macro-editor l)
  (:interactive #t)
  (if (symbol? l) (set! l (symbol->string l)))
  (let* ((b (current-buffer-url))
         (style (get-style-tree))
         (packs (if (tm-atomic? style) (list style) (tm-children style)))
         (styps (list-remove-duplicates (append packs (list "macro-editor"))))
         (u (string-append "tmfs://aux/edit-" l)))
    (and-with def (get-definition l)
      (when (tm-func? (tm-ref def 1) 'macro)
        (dialogue-window (macro-editor b u styps l def)
                         (lambda x (noop))
                         "Macro editor")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching a definition in style files and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-style-definition-in done l name)
  (if (tree-atomic? name) (set! name (tree->string name)))
  (and (string? name)
       (not (in? name done))
       (with t (tree-load-style name)
         (search-style-definition (cons name done) l t))))

(define (search-style-definition-in-list done l packs)
  (first-match (reverse packs)
               (cut search-style-definition-in '() l <>)))

(define (search-style-definition done l t)
  (cond ((and (tree-func? t 'assign 2)
              (tree-atomic? (tree-ref t 0))
              (== (tree->string (tree-ref t 0)) l))
         (car done))
        ((tree-atomic? t) #f)
        ((tree-is? t 'use-package)
         (search-style-definition-in-list done l (tree-children t)))
        ((tree-in? t '(document concat surround with))
         (first-match (reverse (tree-children t))
                      (cut search-style-definition done l <>)))
        (else #f)))

(define (search-style-package l)
  (let* ((style (get-style-tree))
         (packs (if (tm-atomic? style) (list style) (tm-children style))))
    (search-style-definition-in-list '() l packs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct editing of the source of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-assign-macro def)
  (and-with m (tree-ref def 1)
    (if (tree-is? m 'macro)
        (tree-go-to m :last :start)
        (tree-go-to m :start))
    #t))

(define (edit-macro-in-preamble l)
  (with b (buffer-tree)
    (and-with def (get-definition* l b)
      (when (tree-is? (tree-ref b 0) 'hide-preamble)
        (tree-assign-node (tree-ref (buffer-tree) 0) 'show-preamble)
        (with other `(ignore (document ,@(cdr (tree-children b))))
          (tree-remove! b 1 (- (tree-arity b) 1))
          (tree-insert! b 1 (list other))))
      (edit-assign-macro def))))

(define (edit-macro-in-style-file l)
  (and-with name (search-style-package l)
    (let* ((style-name (string-append name ".ts"))
           (style-url (url-append "$TEXMACS_STYLE_PATH" style-name))
           (file-name (url-resolve style-url "r")))
      (load-buffer file-name)
      (delayed
        (:idle 1)
        (and-with def (get-definition** l (buffer-tree))
          (edit-assign-macro def))))))

(tm-define (has-macro-source? l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (get-definition* l (buffer-tree))
      (search-style-package l)))

(tm-define (edit-macro-source l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (edit-macro-in-preamble l)
      (edit-macro-in-style-file l)))
