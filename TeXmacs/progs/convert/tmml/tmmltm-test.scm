
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmltm-test.scm
;; DESCRIPTION : test conversion of Xml trees to TeXmacs trees
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tmml tmmltm-test)
  (:use (convert tmml tmmltm)))

(define (code x)
  `(*TOP* ,x))

(tm-define (regtest-tmmltm)
  (regression-test-group
   "tmmltm" "tmmltm"
   parse-tmml code
   (test "parse ampersand" "&amp" "&")))