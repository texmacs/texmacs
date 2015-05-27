
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-sem-edit.scm
;; DESCRIPTION : semantic mathematical editing
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-sem-edit)
  (:use (math math-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-insert s)
  (:require (in-sem-math?))
  (let* ((ct (cursor-tree))
         (bt (before-cursor))
         (at (after-cursor)))
    (if (not (packrat-correct? "std-math" "Main" ct))
        (former s)
        (begin
          (when (tm-func? bt 'suppressed)
            (tree-cut bt))
          (when (tm-func? at 'suppressed)
            (tree-cut at))
          (former s)
          (when (not (packrat-correct? "std-math" "Main" (cursor-tree)))
            (insert '(suppressed (tiny-box))))))))

(tm-define (kbd-backspace)
  (:require (in-sem-math?))
  (let* ((ct (cursor-tree))
         (bt (before-cursor))
         (at (after-cursor)))
    (if (not (packrat-correct? "std-math" "Main" ct))
        (former)
        (begin
          (when (tm-func? bt 'suppressed)
            (tree-cut bt))
          (when (tm-func? at 'suppressed)
            (tree-cut at))
          (former)
          (when (and (string? bt) (== (math-symbol-type bt) "infix"))
            (insert `(suppressed ,bt)))
          (when (not (packrat-correct? "std-math" "Main" (cursor-tree)))
            (insert '(suppressed (tiny-box))))))))
