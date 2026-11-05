;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-tree.scm
;; DESCRIPTION : TeXmacs tree and stree representation unit tests
;; COPYRIGHT   : (C) 2026  Liza Belos
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tests test-tree)
  (:use (tests test-runner)))

(define-test "tree: atomic tree creation and inspection"
  (let ((t (tree "simple text")))
    (assert-true (tree? t))
    (assert-true (tree-atomic? t))
    (assert-equal "simple text" (tree->string t))))

(define-test "tree: compound tree creation and arity"
  (let ((t (tree 'concat (tree "first") (tree "second"))))
    (assert-true (tree? t))
    (assert-false (tree-atomic? t))
    (assert-equal 'concat (tree-label t))
    (assert-equal 2 (tree-arity t))
    (assert-equal "first" (tree->string (tree-ref t 0)))
    (assert-equal "second" (tree->string (tree-ref t 1)))))

(define-test "tree: stree conversion roundtrip"
  (let* ((st '(document (section "Title") (p "Paragraph text")))
         (t (stree->tree st))
         (roundtrip (tree->stree t)))
    (assert-true (tree? t))
    (assert-equal 'document (tree-label t))
    (assert-equal 2 (tree-arity t))
    (assert-equal st roundtrip)))
