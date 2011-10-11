
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coq-edit.scm
;; DESCRIPTION : editing routines for Coq
;; COPYRIGHT   : (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (coq-edit)
  (:use (utils plugins plugin-eval)
        (link locus-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding unique identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coq-insert-ids-sub done t)
  (when (tree-compound? t)
    (when (tree-in? t '(coq-command coq-enunciation))
      (with s (tree->stree (tree-ref t 0))
        (if (or (== s "") (ahash-ref done s))
            (with new-s (create-unique-id)
              (ahash-set! done new-s #t)
              (tree-set t 0 new-s))
            (ahash-set! done s #t))))
    (for (c (tree-children t))
      (coq-insert-ids-sub done c))))

(tm-define (coq-insert-ids t)
  (with done (make-ahash-table)
    (coq-insert-ids-sub done t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (coq-command-context? t)
  (and (tree-is? t 'coq-command)
       (== (tree-down-index t) 2)))

(tm-define (insert-coq-command)
  (when (not (inside? 'coq-command))
    (with cmd `(coq-command ,(create-unique-id) "dark grey" "")
      (insert-go-to cmd '(2 0)))))

(tm-define (kbd-enter t shift?)
  (:require (coq-command-context? t))
  (with cmd `(coq-command ,(create-unique-id) "dark grey" "")
    (if shift?
        (with-innermost doc 'document
          (with i (+ (tree-down-index doc) 1)
            (tree-insert! doc i (list cmd))
            (tree-go-to doc i 2 0)))
        (begin
          (if (tree-is? t :up 'concat)
              (set! t (tree-up t))
              (tree-set! t `(concat ,t)))
          (with i (+ (tree-down-index t) 1)
            (tree-insert! t i (list cmd))
            (tree-go-to t i 2 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-coq-enunciation type)
  (insert-go-to `(coq-enunciation ,(create-unique-id) "dark grey" ,type
                                  "" (document "") (document ""))
                '(3 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define coq-status-table (make-ahash-table))
(define coq-updating 0)

(tm-define (coq-set-status id status)
  (with col (cond ((== status "error") "red")
                  ((== status "checked") "black")
                  ((== status "unchecked") "dark grey")
                  ((== status "busy") "#8080c0")
                  (else "dark blue"))
    (ahash-set! coq-status-table id col)))

(define (coq-update-status t tab)
  (cond ((null? t) #f)
        ((pair? t)
         (coq-update-status (car t) tab)
         (coq-update-status (cdr t) tab))
        ((tree-atomic? t) #f)
        ((and (tree-in? t '(coq-command coq-enunciation))
              (tree-atomic? (tree-ref t 0))
              (ahash-ref tab (tree->string (tree-ref t 0))))
         (let* ((id (tree->string (tree-ref t 0)))
                (col (ahash-ref tab id)))
           (tree-assign! t `(,(tree-label t) ,id ,col
                             ,@(cddr (tree-children t))))
           (ahash-remove! tab id)))
        (else (coq-update-status (tree-children t) tab))))

(tm-define (coq-update)
  (when (and (!= (ahash-size coq-status-table) 0) (in-coq-style?))
    (coq-update-status (buffer-tree) coq-status-table)))

(tm-define (coq-check)
  (coq-insert-ids (buffer-tree))
  (let* ((cmd (object->string `(check ,(tree->stree (buffer-tree)))))
         (ret (lambda (r)
                (coq-update)
                (delayed (:idle 1000) (coq-update))
                (delayed (:idle 3000) (coq-update))
                (delayed (:idle 10000) (coq-update))
                (set! coq-updating (- coq-updating 1)))))
    (silent-feed "coq" "default" cmd ret '())
    (set! coq-updating (+ coq-updating 1))
    (delayed
      (:while (!= coq-updating 0))
      (:every 1000)
      (coq-update))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (coq-remove-comments t)
  (cond ((and (tree-is? t 'coq-comment) (tree-is? t :up 'document))
         (tree-remove (tree-up t) (tree-index t) 1))
        ((tree-compound? t)
         (for-each coq-remove-comments (reverse (tree-children t))))))
