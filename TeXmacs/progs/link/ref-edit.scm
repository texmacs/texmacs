
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
    (if (path-inf? (tree->path (car l)) (tree->path (cursor-tree)))
        (tree-go-to (car l) :end)
        (list-go-to-previous* (cdr l)))))

(tm-define (list-go-to-previous l)
  (list-go-to-previous* (reverse l)))

(tm-define (list-go-to-next l)
  (when (nnull? l)
    (if (path-inf? (tree->path (cursor-tree)) (tree->path (car l)))
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
;; Finding the label key from its number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (number->labels num)
  (list-filter (find-references num)
               (lambda (x)
                 (not (or (string-starts? x "auto-")
                          (string-starts? x "bib-"))))))

(define (abbr->type s)
  (cond ((in? s '("t" "th" "thm")) "theorem")
        ((in? s '("p" "pr" "prop")) "proposition")
        ((in? s '("l" "le" "lm" "lem")) "lemma")
        ((in? s '("co" "cor" "corr")) "corollary")
        ((in? s '("def" "dfn" "defn")) "definition")
        ((in? s '("not")) "notation")
        ((in? s '("ax")) "axiom")
        ((in? s '("conv")) "convention")
        ((in? s '("conj")) "conjecture")
        ((in? s '("rem")) "remark")
        ((in? s '("war")) "warning")
        ((in? s '("ex")) "example")
        ((in? s '("exc" "exe" "exer")) "exercise")
        ((in? s '("prb" "prob")) "problem")
        ((in? s '("sol")) "solution")
        ((in? s '("c" "ch" "chap")) "chapter")
        ((in? s '("s" "sec")) "section")
        ((in? s '("ss" "ssec" "subs" "subsec")) "subsection")
        ((in? s '("par" "para")) "paragraph")
        ((in? s '("e" "eq" "eqn" "equa")) "equation")
        ((in? s '("fig")) "figure")
        ((in? s '("tab")) "table")
        ((in? s '("alg" "algo")) "algorithm")
        (else s)))

(define (type->types type)
  (cond ((== type "section") (list "section" "subsection" "subsubsection"))
        ((== type "subsection") (list "subsection" "subsubsection"))
        ((== type "paragraph") (list "paragraph subparagraph"))
        ((== type "figure") (list "big-figure" "small-figure"))
        ((== type "table") (list "big-table" "small-table"))
        ((== type "algorithm") (list "algorithm"
                                     "specified-algorithm"
                                     "named-algorithm"
                                     "named-specified-algorithm"))
        ((== type "equation") (list "equation" "eqnarray" "eqnarray*"))
        (else (list type))))

(define (abbr->types s)
  (type->types (abbr->type s)))

(define (previous-word t)
  (cond ((not (and (tree? t) (tree-up t))) #f)
        ((tree-is? (tree-up t) 'concat)
         (and-let* ((p (tree-up t))
                    (i (- (tree-index t) 1)))
           (while (and (>= i 0) (not (tree-atomic? (tree-ref p i))))
             (set! i (- i 1)))
           (and (>= i 0)
                (let* ((s (tm-string-trim-right (tree->string (tree-ref p i))))
                       (j (string-search-backwards " " (string-length s) s)))
                  (if (>= j 0) (substring s (+ j 1) (string-length s)) s)))))
        ((or (tree-atomic? t) (reference-context? t) (tree-is? t 'inactive))
         (previous-word (tree-up t)))
        (else #f)))

(define type-list
  (with l (append (enunciation-tag-list) (section-tag-list)
                  (equation-tag-list) (algorithm-tag-list))
    (append (map symbol->string l) (list "figure" "table"))))

(define (plural s)
  (cond ((== s "") s)
        ((string-ends? s "y")
         (string-append (substring s 0 (- (string-length s) 1)) "ies"))
        (else
         (string-append s "s"))))

(define (word-matches? w t)
  (or (== w t)
      (== w (plural t))
      (== w (translate-from-to t "english" (get-init "language")))
      (== w (translate-from-to (plural t) "english" (get-init "language")))))

(define (word->type s*)
  (with s (locase-all s*)
    (with f (list-find type-list (cut word-matches? s <>))
      (or f (cond ((== s "(") "equation")
                  (else s))))))
    
(define (word->types s)
  ;;(display* "word->types " s "\n")
  (type->types (word->type s)))

(define (label-matches-sub? t types)
  ;;(display* "label-matches-sub? " (tree->path t) ", " types "\n")
  (and (tree? t)
       (or (tree-in? t (map string->symbol types))
           (and (tree-is? t 'concat)
                (or (in? "subsubsection" types) (in? "subparagraph" types))
                (exists? (cut label-matches-sub? <> types)
                         (tree-children t))))))

(define (label-matches? id types)
  ;;(display* "label-matches? " id ", " types "\n")
  (and-with t (and-nnull? (search-label (buffer-tree) id))
    (tree-search-upwards (car t) (cut label-matches-sub? <> types))))

(tm-define (number->label t)
  (and-let* ((s (tm->string t))
             (types (or (with i (string-search-forwards ":" 0 s)
                          (and (>= i 0) (abbr->types (substring s 0 i))))
                        (and-with w (previous-word t)
                          (word->types w))
                        (list)))
             (num (with i (string-search-forwards ":" 0 s)
                    (if (>= i 0) (substring s (+ i 1) (string-length s)) s)))
             (labs (and-nnull? (number->labels num))))
    (if (null? (cdr labs)) (car labs)
        (with f (list-filter labs (lambda (l) (label-matches? l types)))
          (if (null? f) (car labs) (car f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activating references whose keys are inferred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-inactive-reference?)
  (and (in-preview-ref?)
       (tree-atomic? (cursor-tree))
       (reference-context? (tree-up (cursor-tree)))
       (tree-is? (tree-up (tree-up (cursor-tree))) 'inactive)))

(define (label-exists? s)
  (with t (set-of-labels (buffer-tree))
    (ahash-ref t s)))

(tm-define (kbd-return)
  (:require (in-inactive-reference?))
  (with-innermost t reference-context?
    (for (c (tree-children t))
      (and-with s (tm->string c)
        (when (not (label-exists? s))
          (and-with id (number->label c)
            (tree-assign c id)))))
    (former)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Previewing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (preview-reference body body*)
  (:secure #t)
  (and-with ref (tree-up body)
    (with (x1 y1 x2 y2) (tree-bounding-rectangle ref)
      (and-let* ((id (and (tree-atomic? body*) (tree->string body*)))
                 (tip (and id (ref-preview id))))
        (show-tooltip id ref tip "auto" "auto" "default" 0.6)))))

(tm-define (keyboard-press key time)
  (with before? (in-inactive-reference?)
    (former key time)
    (with after? (in-inactive-reference?)
      (when (or before? after?)
        (delayed
          (:idle 100)
          (let* ((id1 (and (in-inactive-reference?)
                           (tm->string (cursor-tree))))
                 (tip1 (and id1 (ref-preview id1)))
                 (id2 (and id1 (not tip1)
                           (number->label (cursor-tree))))
                 (tip2 (and id2 (ref-preview id2)))
                 (id (if tip1 id1 id2))
                 (tip (or tip1 tip2)))
            (if tip
                (begin
                  (show-tooltip id (cursor-tree) tip
                                "auto" "auto" "keyboard" 0.6)
                  (when (not tip1)
                    (set-message `(concat (verbatim ,id1) " -> "
                                          (verbatim ,id2)) "")))
                (close-tooltip))))))))
