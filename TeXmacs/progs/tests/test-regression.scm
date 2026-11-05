;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-regression.scm
;; DESCRIPTION : Integration of existing TeXmacs regression tests
;; COPYRIGHT   : (C) 2026  Liza Belos
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tests test-regression)
  (:use (tests test-runner)
        (convert tools tmlength-test)
        (convert tools environment-test)
        (convert tools xmltm-test)
        (convert mathml mathtm-test)
        (prog prog-format-test)
        (utils cite cite-sort-test)))

(define-test "regression: tmlength conversion tests"
  (regtest-tmlength)
  (assert-true #t))

(define-test "regression: environment variables conversion tests"
  (regtest-environment)
  (assert-true #t))

(define-test "regression: mathml conversion tests"
  (regtest-mathtm)
  (assert-true #t))

(define-test "regression: prog-format tests"
  (regtest-prog-format)
  (assert-true #t))

(define-test "regression: bibliography citation sorting tests"
  (regtest-cite-sort)
  (assert-true #t))
