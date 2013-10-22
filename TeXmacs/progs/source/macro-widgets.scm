
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-widgets.scm
;; DESCRIPTION : widgets for editing macros
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-widgets)
  (:use (source macro-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget for editing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macro-retrieve* u)
  (and-with t (buffer-get-body u)
    (if (tm-is? t 'document) (set! t (tm-ref t :last)))
    (and (tm-in? t '(edit-macro edit-tag)) t)))

(define (macro-retrieve u)
  (and-with t (macro-retrieve* u)
    (cond ((tm-is? (tree-ref t :last) 'inactive*)
           `(,(tm-label t) ,@(cDr (tm-children t)) ,(tree-ref t :last 0)))
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

(define (buffer-get-preamble buf)
  (if (buffer-has-preamble? buf)
      (tree-ref buf 0 0)
      `(document "")))

(define (preamble-insert pre ass)
  (with m (list-find (reverse (tree-children pre))
                     (lambda (x)
                       (and (tree-is? x 'assign)
                            (tm-equal? (tm-ref x 0) (tm-ref ass 0)))))
    (if m
        (tree-set m ass)
        (tree-insert pre (tree-arity pre) (list ass)))))

(define (macro-apply* u t)
  (let* ((b   (buffer-get-master u))
         (m   (buffer-get-master b))
         (buf (buffer-get-body b))
         (l   (tree->string (tm-ref t 0)))
         (old (get-definition* l buf))
         (mac (if (tm-is? t 'edit-macro)
                  `(macro ,@(cdr (tm-children t)))
                  (tm-ref t 1)))
         (new `(assign ,l ,mac)))
    (cond ((or (not (buffer-exists? u)) (not (buffer-exists? b))) #f)
          ((and old (tree->path old)) (tree-set old 1 mac))
          (else
            (when (not (buffer-has-preamble? buf))
              (tree-insert! buf 0 '((hide-preamble (document "")))))
            (when (buffer-has-preamble? buf)
              (with pre (tree-ref buf 0 0)
                (preamble-insert pre new)))
            (when (!= m b)
              (macro-apply* b t))))))

(define (macro-apply u)
  (and-with t (macro-retrieve u)
    (macro-apply* u t)))

(tm-widget ((macro-editor u packs doc) quit)
  (padded
    (resize "600px" "300px"
      (texmacs-input doc `(style (tuple ,@packs)) u))
    ===
    (hlist
      (enum (set-macro-mode u answer)
            '("Text" "Source")
            "Text" "6em")
      >>
      (explicit-buttons
        ("Apply" (macro-apply u))
        //
        ("Ok" (begin (macro-apply u) (quit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (editable-macro? l)
  (if (symbol? l) (set! l (symbol->string l)))
  (get-definition l))

(tm-define (open-macro-editor l)
  (:interactive #t)
  (if (symbol? l) (set! l (symbol->string l)))
  (let* ((b (current-buffer-url))
         (packs (get-style-list))
         (styps (list-remove-duplicates (append packs (list "macro-editor"))))
         (u (string-append "tmfs://aux/edit-" l)))
    (and-with def (get-definition l)
      (when (and (tm-func? def 'assign 2)
		 (tm-equal? (tm-ref def 1) '(uninit)))
	(set! def `(assign ,(tm-ref def 0) (macro ""))))
      (let* ((mac (if (tm-func? (tm-ref def 1) 'macro)
                      `(edit-macro ,l ,@(tm-children (tm-ref def 1)))
                      `(edit-tag ,l ,(tm-ref def 1))))
             (pre (buffer-get-preamble (buffer-tree)))
             (doc `(document (hide-preamble ,pre) ,mac)))
        (dialogue-window (macro-editor u styps doc)
                         (lambda x (noop))
                         "Macro editor")
        (buffer-set-master u b)))))

(tm-define (open-macros-editor)
  (:interactive #t)
  (noop))
