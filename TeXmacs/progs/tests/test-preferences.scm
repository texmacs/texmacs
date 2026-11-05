;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-preferences.scm
;; DESCRIPTION : TeXmacs preferences unit tests
;; COPYRIGHT   : (C) 2026  Liza Belos
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tests test-preferences)
  (:use (tests test-runner)))

(define-test "preferences: default value fallback"
  (let ((val (get-preference "non_existent_test_key_xyz_123")))
    (assert-equal "default" val)))

(define-test "preferences: set and get roundtrip"
  (let ((test-key "test_preference_roundtrip_key")
        (test-val "custom_test_val_999"))
    (set-preference test-key test-val)
    (assert-equal test-val (get-preference test-key))))
