
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ref-edit.scm
;; DESCRIPTION : editing routines for references
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link ref-edit)
  (:use (generic generic-edit)
        (generic document-part)
        (text text-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding all standard types of labels/references in a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (label-context? t)
  (tree-in? t '(label)))

(define (reference-context? t)
  (tree-in? t '(reference eqref pageref)))

(define ((named-context? pred? id) t)
  (and (pred? t)
       (exists? (cut tm-equal? <> id) (tm-children t))))

(tm-define (search-labels t)
  (tree-search t label-context?))

(tm-define (search-label t id)
  (tree-search t (named-context? label-context? id)))

(tm-define (search-references t)
  (tree-search t reference-context?))

(tm-define (search-reference t id)
  (tree-search t (named-context? reference-context? id)))

(define (and-nnull? l)
  (and (nnull? l) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preview of the content that a reference points to
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (preview-context? t)
  (or (tree-is? t 'row)
      (and (tree-up t) (tree-is? (tree-up t) 'document))))

(define (math-context? t)
  (tree-in? t '(equation equation* eqnarray eqnarray*)))

(define (uncell t)
  (if (tm-func? t 'cell 1) (tm-ref t 0) t))

(define (clean-preview t)
  (cond ((tm-is? t 'document)
         `(document ,@(map clean-preview (tm-children t))))
        ((tm-is? t 'concat)
         (apply tmconcat (map clean-preview (tm-children t))))
        ((tm-in? t (section-tag-list))
         (with l (symbol-append (tm-label t) '*)
           `(,l ,@(tm-children t))))
        ((tm-in? t '(label item item* bibitem bibitem* eq-number)) "")
        ((or (tm-func? t 'equation 1) (tm-func? t 'equation* 1))
         `(equation* ,(clean-preview (tm-ref t 0))))
        ((tm-in? t '(eqnarray eqnarray* tformat table row cell))
         `(,(tm-label t) ,@(map clean-preview (tm-children t))))
        (else t)))

(define (preview-expand-context? t)
  (tree-in? t '(theorem proposition lemma corollary conjecture
                theorem* proposition* lemma* corollary* conjecture*
                definition axiom
                definition* axiom*)))

(define (label-preview t)
  (and-with doc (tree-search-upwards t preview-context?)
    (with math? (tree-search-upwards t math-context?)
      (when (and (tree-up doc) (tree-up (tree-up doc))
                 (tree-is? (tree-up doc) 'document)
                 (preview-expand-context? (tree-up (tree-up doc))))
        (set! doc (tree-up doc)))
      (when (tm-is? doc 'row)
        (set! doc (apply tmconcat (map uncell (tm-children doc)))))
      (set! doc (clean-preview doc))
      (when math?
        (set! doc `(with "math-display" "true" (math ,doc))))
      `(preview-balloon ,doc))))

(tm-define (ref-preview id)
  (and-with l (and-nnull? (search-label (buffer-tree) id))
    (label-preview (car l))))

(define (preview-init? p)
  (and-with x (and (pair? p) (car p))
    (and (string? x)
         (not (string-starts? x "page-"))
         (nin? x (list "zoom-factor" "full-screen-mode")))))

(tm-define (preview-reference body body*)
  (:secure #t)
  (and-with ref (tree-up body)
    (with (x1 y1 x2 y2) (tree-bounding-rectangle ref)
      (and-let* ((packs (get-style-list))
                 (pre (document-get-preamble (buffer-tree)))
                 (id (and (tree-atomic? body*) (tree->string body*)))
                 (balloon (ref-preview id))
                 (zf (get-window-zoom-factor))
                 (sf (/ 4.0 zf))
                 (mag (number->string (/ zf 1.5)))
                 (inits* (map cdr (cdr (tm->stree (get-all-inits)))))
                 (inits (list-filter inits* preview-init?))
                 (env (apply append inits))
                 (balloon* `(with ,@env "magnification" ,mag ,balloon))
                 (w (widget-texmacs-output
                     `(surround (hide-preamble ,pre) "" ,balloon*)
                     `(style (tuple ,@packs)))))
        (show-balloon w x1 (- y1 1280))))))
