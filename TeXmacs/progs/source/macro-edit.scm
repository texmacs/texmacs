
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
;; Widget for editing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((macro-editor l args body) cmd)
  (padded
    (form "Macro"
      (hlist
        (bold (text l))
        //
        (for (i (.. 0 (length args)))
          //
          (form-input (string-append "arg" (number->string i)) "string"
                      (list (list-ref args i)) "1w")))
      ======
      (resize "500px" "300px"
        (texmacs-input `(document ,body) '(style "macro-editor") (noop) #f)))))

(tm-define (open-macro-editor l)
  (:interactive #t)
  (if (symbol? l) (set! l (symbol->string l)))
  (and-with def (get-definition l)
    (when (tm-func? def 'macro)
      (with args (map tree->string (cDr (tree-children def)))
        (dialogue-window (macro-editor l args (tree-ref def :last))
                         noop "Macro editor")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct editing of the source of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (edit-macro-source l)
  (noop))
