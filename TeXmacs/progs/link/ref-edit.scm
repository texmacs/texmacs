
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
  (:use (utils edit variants)
        (generic generic-edit)
        (generic document-part)
        (text text-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding all standard types of labels/references in a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (label-context? t)
  (tree-in? t (label-tag-list)))

(tm-define (reference-context? t)
  (tree-in? t (reference-tag-list)))

(tm-define (citation-context? t)
  (tree-in? t (citation-tag-list)))

(tm-define (tie-context? t)
  (or (label-context? t) (reference-context? t) (citation-context? t)))

(define ((named-context? pred? . ids) t)
  (and (pred? t)
       (exists? (lambda (id)
                  (exists? (cut tm-equal? <> id) (tm-children t)))
                ids)))

(define (and-nnull? l)
  (and (nnull? l) l))

(tm-define (search-labels t)
  (tree-search t label-context?))

(tm-define (search-label t id)
  (tree-search t (named-context? label-context? id)))

(tm-define (search-references t)
  (tree-search t reference-context?))

(tm-define (search-reference t id)
  (tree-search t (named-context? reference-context? id)))

(tm-define (search-citations t)
  (tree-search t citation-context?))

(tm-define (search-citation t id)
  (tree-search t (named-context? citation-context? id)))

(tm-define (search-tie t id)
  (let* ((id1 (if (string-starts? id "bib-") (string-drop id 4) id))
         (id2 (string-append "bib-" id1)))
    (tree-search t (named-context? tie-context? id1 id2))))

(tm-define (search-duplicate-labels t)
  (let* ((labs (search-labels t))
         (labl (map (lambda (lab) (tm->string (tm-ref lab 0))) labs))
         (freq (list->frequencies labl))
         (filt (lambda (lab)
                 (with f (ahash-ref freq (tm->string (tm-ref lab 0)))
                   (> (or f 0) 1)))))
    (list-filter labs filt)))

(define (tm-keys t)
  (cond ((tm-in? t '(cite-detail)) (list (tm-ref t 0)))
        (else (tm-children t))))

(define ((tie-in? t) ref)
  (with l (map tm->string (tm-keys ref))
    (forall? (lambda (s) (ahash-ref t s)) l)))

(define (strip-bib s)
  (if (string-starts? s "bib-") (string-drop s 4) s))

(define (set-of-labels t)
  (let* ((labs (search-labels t))
         (labl (map (lambda (t) (strip-bib (tm->string (tm-ref t 0)))) labs))
         (labt (list->ahash-set labl)))
    (if (project-attached?)
        (let* ((glob (list->ahash-set (map strip-bib (list-references* #t))))
               (loc  (list->ahash-set (map strip-bib (list-references)))))
          (ahash-table-append (ahash-table-difference glob loc) labt))
        labt)))

(tm-define (search-broken-references t)
  (let* ((refs (search-references t))
         (labt (set-of-labels t)))
    (list-filter refs (non (tie-in? labt)))))

(tm-define (search-broken-citations t)
  (let* ((refs (search-citations t))
         (labt (set-of-labels t)))
    (list-filter refs (non (tie-in? labt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-go-to-first l)
  (tree-go-to (car l) :end))

(tm-define (list-go-to-last l)
  (tree-go-to (cAr l) :end))

(define (list-go-to-previous* l)
  (when (nnull? l)
    (if (path-less? (tree->path (car l)) (tree->path (cursor-tree)))
        (tree-go-to (car l) :end)
        (list-go-to-previous* (cdr l)))))

(tm-define (list-go-to-previous l)
  (list-go-to-previous* (reverse l)))

(tm-define (list-go-to-next l)
  (when (nnull? l)
    (if (path-less? (tree->path (cursor-tree)) (tree->path (car l)))
        (tree-go-to (car l) :end)
        (list-go-to-next (cdr l)))))

(tm-define (list-go-to l dir)
  (cond ((nlist? l) (noop))
        ((== dir :first) (list-go-to-first l))
        ((== dir :last) (list-go-to-last l))
        ((== dir :previous) (list-go-to-previous l))
        ((== dir :next) (list-go-to-next l))))

(define current-id '(none))

(tm-define (tie-id)
  (and-with t (tree-innermost tie-context? #t)
    (or (and (exists? (cut tm-equal? <> current-id) (tm-children t))
             current-id)
        (and (tm-atomic? (tm-ref t 0))
             (with key (tm->string (tm-ref t 0))
               (if (string-starts? key "bib-") (string-drop key 4) key))))))

(tm-define (same-ties)
  (and-nnull? (search-tie (buffer-tree) (tie-id))))

(tm-define (duplicate-labels)
  (and-nnull? (search-duplicate-labels (buffer-tree))))

(tm-define (broken-references)
  (and-nnull? (search-broken-references (buffer-tree))))

(tm-define (broken-citations)
  (and-nnull? (search-broken-citations (buffer-tree))))

(tm-define (go-to-same-tie dir)
  (:applicable (same-ties))
  (set! current-id (tie-id))
  (list-go-to (same-ties) dir))

(tm-define (go-to-duplicate-label dir)
  (:applicable (duplicate-labels))
  (list-go-to (duplicate-labels) dir))

(tm-define (go-to-broken-reference dir)
  (:applicable (broken-references))
  (list-go-to (broken-references) dir))

(tm-define (go-to-broken-citation dir)
  (:applicable (broken-citations))
  (list-go-to (broken-citations) dir))

(tm-define (special-extremal t forwards?)
  (:require (focus-label t))
  (with lab (focus-label t)
    (tree-go-to lab :end)
    (special-extremal lab forwards?)))

(tm-define (special-incremental t forwards?)
  (:require (focus-label t))
  (with lab (focus-label t)
    (tree-go-to lab :end)
    (special-incremental lab forwards?)))

(tm-define (special-extremal t forwards?)
  (:require (tie-context? t))
  (go-to-same-tie (if forwards? :last :first)))

(tm-define (special-incremental t forwards?)
  (:require (tie-context? t))
  (go-to-same-tie (if forwards? :next :previous)))

(tm-define (special-navigate t dir)
  (:require (label-context? t))
  (go-to-duplicate-label dir))

(tm-define (special-navigate t dir)
  (:require (reference-context? t))
  (go-to-broken-reference dir))

(tm-define (special-navigate t dir)
  (:require (citation-context? t))
  (go-to-broken-citation dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate preview document of the content that a reference points to
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
                 (tree-is? (tree-up doc) 'document))
        (with enc (tree-up (tree-up doc))
          (cond ((preview-expand-context? enc)
                 (set! doc (tree-up doc)))
                ((and (tree-in? enc (algorithm-tag-list))
                      (< (tree-index (tree-up doc)) (- (tree-arity enc) 1)))
                 (set! doc `(with "par-first" "0em" ,(tree-up doc)))))))
      (when (tm-is? doc 'row)
        (set! doc (apply tmconcat (map uncell (tm-children doc)))))
      (set! doc (clean-preview doc))
      (when math?
        (set! doc `(with "math-display" "true" (math ,doc))))
      `(preview-balloon ,doc))))

(tm-define (ref-preview id)
  (and-with p (and-nnull? (label->path id))
    (with t (path->tree (cDr p))
      (cond ((label-context? t)
             (label-preview t))
            ((tree-in? t '(glossary glossary-explain glossary-dup
                           index subindex subsubindex index-complex))
             (label-preview t))
            (else #f)))))

(define (preview-init? p)
  (and-with x (and (pair? p) (car p))
    (and (string? x)
         (not (string-starts? x "page-"))
         (nin? x (list "zoom-factor" "full-screen-mode")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displaying the preview tooltip window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define preview-id #f)
(define preview-win #f)
(define preview-unmap? #f)
(define preview-settings #f)

(define (preview-confirm settings)
  ;;(display* "Confirm " preview-win "\n")
  (set! preview-unmap? #f)
  (set! preview-settings settings))

(define (preview-unmap)
  ;;(display* "Unmap " preview-win "\n")
  (alt-window-hide preview-win)
  (alt-window-delete preview-win)
  (set! preview-id #f)
  (set! preview-win #f)
  (set! preview-unmap? #f)
  (set! preview-settings #f))

(define (preview-map wid x y id settings)
  (set! x (quotient x 256))
  (set! y (quotient y 256))
  (if preview-win (preview-unmap))
  (with win (alt-window-handle)
    (alt-window-create-tooltip win wid (translate "Preview"))
    (alt-window-set-position win x y)
    (alt-window-show win)
    (set! preview-id id)
    (set! preview-win win)
    (set! preview-unmap? #f)
    (set! preview-settings settings)
    ;;(display* "Map " preview-win "\n")
    ))

(tm-define (mouse-event key x y mods time)
  (:require (and preview-win (not preview-unmap?)))
  ;;(display* "Mouse event " key ", " x ", " y "; " time "\n")
  (with (x1 y1 x2 y2 mx my sx sy zf) preview-settings
    (let* ((xx (inexact->exact (round (/ (- x sx) (/ 5.0 zf)))))
           (yy (inexact->exact (round (/ (- y sy) (/ 5.0 zf)))))
           (dx (quotient (abs (- xx mx)) 256))
           (dy (quotient (abs (- yy my)) 256))
           (d  (* 5 256)))
      (when (or (!= key "move")
                (< xx (- x1 d)) (> xx (+ x2 d))
                (< yy (- y1 d)) (> yy (+ y2 d))
                (> dx 10) (> dy 10))
        (set! preview-unmap? preview-win)
        ;;(display* "Schedule unmap " preview-win "\n")
        (delayed
          (:pause 250)
          (when (and preview-unmap? (== preview-unmap? preview-win))
            (preview-unmap))))))
  (former key x y mods time))

(tm-define (preview-reference body body*)
  (:secure #t)
  (and-with ref (tree-up body)
    (with (x1 y1 x2 y2) (tree-bounding-rectangle ref)
      (let* ((settings (list (- x1 (get-canvas-x))
                             (- y1 (get-canvas-y))
                             (- x2 (get-canvas-x))
                             (- y2 (get-canvas-y))
                             (- (car  (get-mouse-position)) (get-canvas-x))
                             (- (cadr (get-mouse-position)) (get-canvas-y))
                             (get-scroll-x)
                             (get-scroll-y)
                             (get-window-zoom-factor)))
             (id (and (tree-atomic? body*) (tree->string body*))))
        (if (and preview-win id (== id preview-id))
            (preview-confirm settings)
            (and-let* ((wx (get-window-x))
                       (wy (get-window-y))
                       (packs (get-style-list))
                       (pre (document-get-preamble (buffer-tree)))
                       (balloon (and id (ref-preview id)))
                       (zf (get-window-zoom-factor))
                       (sf (/ 4.0 zf))
                       (mag (number->string (/ zf 1.5)))
                       (inits* (map cdr (cdr (tm->stree (get-all-inits)))))
                       (inits (list-filter inits* preview-init?))
                       (env (apply append inits))
                       (balloon* `(with ,@env "magnification" ,mag ,balloon))
                       (doc `(surround (hide-preamble ,pre) "" ,balloon*))
                       (master (url->system (current-buffer)))
                       (w (widget-texmacs-output
                           `(with "project" ,master ,doc)
                           `(style (tuple ,@packs)))))
              ;;(display* "balloon= " balloon* "\n")
              ;;(display* "size= " (texmacs-widget-size w) "\n")
              ;;(show-balloon w x1 (- y1 1280))
              (preview-map w
                           (+ wx x1)
                           (+ wy y1 -1280)
                           id
                           settings)))))))
