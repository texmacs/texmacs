
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

(texmacs-module (source macro-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the definition of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first-match l pred?)
  (cond ((null? l) #f)
        ((pred? (car l)) (pred? (car l)))
        (else (first-match (cdr l) pred?))))

(define (search-definition l t)
  (cond ((and (tm-func? t 'assign 2)
              (tree-atomic? (tree-ref t 0))
              (== (tree->string (tree-ref t 0)) l))
         (tree-ref t 1))
        ((tree-atomic? t) #f)
        (else (first-match (reverse (tree-children t))
                           (cut search-definition l <>)))))

(define (get-definition* l t)
  (cond ((tm-func? t 'hide-preamble 1) (search-definition l t))
        ((tm-func? t 'show-preamble 1) (search-definition l t))
        ((tree-atomic? t) #f)
        ((tree-in? t '(document surround with))
         (first-match (tree-children t) (cut get-definition* l <>)))
        (else #f)))

(tm-define (get-definition l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (get-definition* l (buffer-tree))
      (get-init-tree l)))

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


(tm-define (ext-edit-macro* a)
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
     (inactive* ,body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget for editing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macro-retrieve u)
  (and-with t (buffer-get-body u)
    (if (tree-is? t 'document) (set! t (tree-ref t 0)))
    (and (tree-in? t '(edit-macro edit-macro*)) t)))

(define (macro-source-mode? u)
  (and-with t (macro-retrieve u)
    (tree-is? t 'edit-macro*)))

(define (macro-toggle-source-mode u)
  (and-with t (macro-retrieve u)
    (cond ((tree-is? t 'edit-macro) (tree-assign-node! t 'edit-macro*))
          ((tree-is? t 'edit-macro*) (tree-assign-node! t 'edit-macro)))))

(define (macro-apply u)
  (display* "u= " u "\n")
  (display* "t= " (buffer-get-body u) "\n"))

(tm-widget ((macro-editor u l args body) quit)
  (padded
    (resize "500px" "300px"
      (texmacs-input
        `(document
           (edit-macro ,l ,@args ,body))
        '(style "macro-editor")
        u))
    (bottom-buttons
      (toggle (macro-toggle-source-mode u) (macro-source-mode? u))
      ///
      (minibar (text "Source"))
      >>
      ("Apply" (macro-apply u))
      //
      ("Ok" (begin (macro-apply u) (quit))))))

(tm-define (open-macro-editor l)
  (:interactive #t)
  (if (symbol? l) (set! l (symbol->string l)))
  (with u (string-append "tmfs://aux/edit-" l)
    (and-with def (get-definition l)
      (when (tm-func? def 'macro)
        (with args (map tree->string (cDr (tree-children def)))
          (dialogue-window (macro-editor u l args (tree-ref def :last))
                           (lambda x (display* "x= " x "\n"))
                           "Macro editor"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct editing of the source of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edit-macro-source l)
  (noop))
