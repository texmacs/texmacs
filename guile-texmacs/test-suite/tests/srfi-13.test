;;;; srfi-13.test --- Test suite for Guile's SRFI-13 functions. -*- scheme -*-
;;;; Martin Grabmueller, 2001-05-07
;;;;
;;;; Copyright (C) 2001, 2004, 2005, 2006 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301 USA

(define-module (test-strings)
  #:use-module (test-suite lib)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14))


(define exception:strict-infix-grammar
  (cons 'misc-error "^strict-infix"))

;; Create a string from integer char values, eg. (string-ints 65) => "A"
(define (string-ints . args)
  (apply string (map integer->char args)))


;;;
;;; string-any
;;;

(with-test-prefix "string-any"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-any 123 "abcde"))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-any "zzz" "abcde")))

  (with-test-prefix "char"

    (pass-if "no match"
      (not (string-any #\C "abcde")))

    (pass-if "one match"
      (string-any #\C "abCde"))

    (pass-if "more than one match"
      (string-any #\X "abXXX"))

    (pass-if "no match, start index"
      (not (string-any #\A "Abcde" 1)))

    (pass-if "one match, start index"
      (string-any #\C "abCde" 1))

    (pass-if "more than one match, start index"
      (string-any #\X "abXXX" 1))

    (pass-if "no match, start and end index"
      (not (string-any #\X "XbcdX" 1 4)))

    (pass-if "one match, start and end index"
      (string-any #\C "abCde" 1 4))

    (pass-if "more than one match, start and end index"
      (string-any #\X "abXXX" 1 4)))

  (with-test-prefix "charset"

    (pass-if "no match"
      (not (string-any char-set:upper-case "abcde")))

    (pass-if "one match"
      (string-any char-set:upper-case "abCde"))

    (pass-if "more than one match"
      (string-any char-set:upper-case "abCDE"))

    (pass-if "no match, start index"
      (not (string-any char-set:upper-case "Abcde" 1)))

    (pass-if "one match, start index"
      (string-any char-set:upper-case "abCde" 1))

    (pass-if "more than one match, start index"
      (string-any char-set:upper-case "abCDE" 1))

    (pass-if "no match, start and end index"
      (not (string-any char-set:upper-case "AbcdE" 1 4)))

    (pass-if "one match, start and end index"
      (string-any char-set:upper-case "abCde" 1 4))

    (pass-if "more than one match, start and end index"
      (string-any char-set:upper-case "abCDE" 1 4)))

  (with-test-prefix "pred"

    (pass-if "no match"
      (not (string-any char-upper-case? "abcde")))

    (pass-if "one match"
      (string-any char-upper-case? "abCde"))

    (pass-if "more than one match"
      (string-any char-upper-case? "abCDE"))

    (pass-if "no match, start index"
      (not (string-any char-upper-case? "Abcde" 1)))

    (pass-if "one match, start index"
      (string-any char-upper-case? "abCde" 1))

    (pass-if "more than one match, start index"
      (string-any char-upper-case? "abCDE" 1))

    (pass-if "no match, start and end index"
      (not (string-any char-upper-case? "AbcdE" 1 4)))

    (pass-if "one match, start and end index"
      (string-any char-upper-case? "abCde" 1 4))

    (pass-if "more than one match, start and end index"
      (string-any char-upper-case? "abCDE" 1 4))))

;;;
;;; string-append/shared
;;;

(with-test-prefix "string-append/shared"

  (pass-if "no args"
    (string=? "" (string-append/shared)))

  (with-test-prefix "one arg"
    (pass-if "empty"
      (string=? "" (string-append/shared "")))
    (pass-if "non-empty"
      (string=? "xyz" (string-append/shared "xyz"))))

  (with-test-prefix "two args"
    (pass-if (string=? ""       (string-append/shared ""    "")))
    (pass-if (string=? "xyz"    (string-append/shared "xyz" "")))
    (pass-if (string=? "xyz"    (string-append/shared ""    "xyz")))
    (pass-if (string=? "abcxyz" (string-append/shared "abc" "xyz"))))

  (with-test-prefix "three args"
    (pass-if (string=? ""   	(string-append/shared ""   ""   "")))
    (pass-if (string=? "xy" 	(string-append/shared "xy" ""   "")))
    (pass-if (string=? "xy" 	(string-append/shared ""   "xy" "")))
    (pass-if (string=? "abxy"   (string-append/shared "ab" "xy" "")))
    (pass-if (string=? "ab"     (string-append/shared ""   ""   "ab")))
    (pass-if (string=? "xyab"   (string-append/shared "xy" ""   "ab")))
    (pass-if (string=? "xyab"   (string-append/shared ""   "xy" "ab")))
    (pass-if (string=? "ghxyab" (string-append/shared "gh" "xy" "ab"))))

  (with-test-prefix "four args"
    (pass-if (string=? ""   	(string-append/shared ""   ""   ""   "")))
    (pass-if (string=? "xy" 	(string-append/shared "xy" ""   ""   "")))
    (pass-if (string=? "xy" 	(string-append/shared ""   "xy" ""   "")))
    (pass-if (string=? "xy" 	(string-append/shared ""   ""   "xy" "")))
    (pass-if (string=? "xy" 	(string-append/shared ""   ""   ""   "xy")))

    (pass-if (string=? "abxy"   (string-append/shared "ab" "xy" ""   "")))
    (pass-if (string=? "abxy"   (string-append/shared "ab" ""   "xy" "")))
    (pass-if (string=? "abxy"   (string-append/shared "ab" ""   ""   "xy")))
    (pass-if (string=? "abxy"   (string-append/shared ""   "ab" ""   "xy")))
    (pass-if (string=? "abxy"   (string-append/shared ""   ""   "ab" "xy")))))

;;;
;;; string-concatenate
;;;

(with-test-prefix "string-concatenate"

  (pass-if-exception "inum" exception:wrong-type-arg
    (string-concatenate 123))

  (pass-if-exception "symbol" exception:wrong-type-arg
    (string-concatenate 'x))

  (pass-if-exception "improper 1" exception:wrong-type-arg
    (string-concatenate '("a" . "b")))

  (pass-if (equal? "abc" (string-concatenate '("a" "b" "c")))))

;;
;; string-compare
;;

(with-test-prefix "string-compare"

  (pass-if "same as char<?"
    (eq? (char<? (integer->char 0) (integer->char 255))
	 (string-compare (string-ints 0) (string-ints 255)
			 (lambda (pos) #t)     ;; lt
			 (lambda (pos) #f)     ;; eq
			 (lambda (pos) #f))))) ;; gt

;;
;; string-compare-ci
;;

(with-test-prefix "string-compare-ci"

  (pass-if "same as char-ci<?"
    (eq? (char-ci<? (integer->char 0) (integer->char 255))
	 (string-compare-ci (string-ints 0) (string-ints 255)
			 (lambda (pos) #t)     ;; lt
			 (lambda (pos) #f)     ;; eq
			 (lambda (pos) #f))))) ;; gt

;;;
;;; string-concatenate/shared
;;;

(with-test-prefix "string-concatenate/shared"

  (pass-if-exception "inum" exception:wrong-type-arg
    (string-concatenate/shared 123))

  (pass-if-exception "symbol" exception:wrong-type-arg
    (string-concatenate/shared 'x))

  (pass-if-exception "improper 1" exception:wrong-type-arg
    (string-concatenate/shared '("a" . "b")))

  (pass-if (equal? "abc" (string-concatenate/shared '("a" "b" "c")))))

;;;
;;; string-every
;;;

(with-test-prefix "string-every"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-every 123 "abcde"))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-every "zzz" "abcde")))

  (with-test-prefix "char"

    (pass-if "empty string"
      (string-every #\X ""))

    (pass-if "empty substring"
      (string-every #\X "abc" 1 1))

    (pass-if "no match at all"
      (not (string-every #\X "abcde")))

    (pass-if "not all match"
      (not (string-every #\X "abXXX")))

    (pass-if "all match"
      (string-every #\X "XXXXX"))

    (pass-if "no match at all, start index"
      (not (string-every #\X "Xbcde" 1)))

    (pass-if "not all match, start index"
      (not (string-every #\X "XXcde" 1)))

    (pass-if "all match, start index"
      (string-every #\X "aXXXX" 1))

    (pass-if "no match at all, start and end index"
      (not (string-every #\X "XbcdX" 1 4)))

    (pass-if "not all match, start and end index"
      (not (string-every #\X "XXcde" 1 4)))

    (pass-if "all match, start and end index"
      (string-every #\X "aXXXe" 1 4)))

  (with-test-prefix "charset"

    (pass-if "empty string"
      (string-every char-set:upper-case ""))

    (pass-if "empty substring"
      (string-every char-set:upper-case "abc" 1 1))

    (pass-if "no match at all"
      (not (string-every char-set:upper-case "abcde")))

    (pass-if "not all match"
      (not (string-every char-set:upper-case "abCDE")))

    (pass-if "all match"
      (string-every char-set:upper-case "ABCDE"))

    (pass-if "no match at all, start index"
      (not (string-every char-set:upper-case "Abcde" 1)))

    (pass-if "not all match, start index"
      (not (string-every char-set:upper-case "ABcde" 1)))

    (pass-if "all match, start index"
      (string-every char-set:upper-case "aBCDE" 1))

    (pass-if "no match at all, start and end index"
      (not (string-every char-set:upper-case "AbcdE" 1 4)))

    (pass-if "not all match, start and end index"
      (not (string-every char-set:upper-case "ABcde" 1 4)))

    (pass-if "all match, start and end index"
      (string-every char-set:upper-case "aBCDe" 1 4)))

  (with-test-prefix "pred"

    ;; in guile 1.6.4 and earlier string-every incorrectly returned #f on an
    ;; empty string
    (pass-if "empty string"
      (string-every char-upper-case? ""))
    (pass-if "empty substring"
      (string-every char-upper-case? "abc" 1 1))

    (pass-if "no match at all"
      (not (string-every char-upper-case? "abcde")))

    (pass-if "not all match"
      (not (string-every char-upper-case? "abCDE")))

    (pass-if "all match"
      (string-every char-upper-case? "ABCDE"))

    (pass-if "no match at all, start index"
      (not (string-every char-upper-case? "Abcde" 1)))

    (pass-if "not all match, start index"
      (not (string-every char-upper-case? "ABcde" 1)))

    (pass-if "all match, start index"
      (string-every char-upper-case? "aBCDE" 1))

    (pass-if "no match at all, start and end index"
      (not (string-every char-upper-case? "AbcdE" 1 4)))

    (pass-if "not all match, start and end index"
      (not (string-every char-upper-case? "ABcde" 1 4)))

    (pass-if "all match, start and end index"
      (string-every char-upper-case? "aBCDe" 1 4))))

(with-test-prefix "string-tabulate"

  (with-test-prefix "bad proc"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-tabulate 123 10))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-tabulate "zzz" 10)))

  (pass-if "static fill-char"
    (string=? (string-tabulate (lambda (idx) #\!) 10) "!!!!!!!!!!"))

  (pass-if "variable fill-char"
    (string=? (string-tabulate
	       (lambda (idx) (integer->char (+ idx 32))) 10) " !\"#$%&'()")))

(with-test-prefix "string->list"

  (pass-if "empty"
     (zero? (length (string->list ""))))

  (pass-if "nonempty"
     (= (length (string->list "foo")) 3))

  (pass-if "empty, start index"
     (zero? (length (string->list "foo" 3 3))))

   (pass-if "nonempty, start index"
     (= (length (string->list "foo" 1 3)) 2))
  )

(with-test-prefix "reverse-list->string"

  (pass-if "empty"
     (string-null? (reverse-list->string '())))

  (pass-if "nonempty"
     (string=? "foo" (reverse-list->string '(#\o #\o #\f)))))


(with-test-prefix "string-join"

  (pass-if "empty list, no delimiter, implicit infix, empty 1"
     (string=? "" (string-join '())))

  (pass-if "empty string, no delimiter, implicit infix, empty 2"
     (string=? "" (string-join '(""))))

  (pass-if "non-empty, no delimiter, implicit infix"
     (string=? "bla" (string-join '("bla"))))

  (pass-if "empty list, implicit infix, empty 1"
     (string=? "" (string-join '() "|delim|")))

  (pass-if "empty string, implicit infix, empty 2"
     (string=? "" (string-join '("") "|delim|")))

  (pass-if "non-empty, implicit infix"
     (string=? "bla" (string-join '("bla") "|delim|")))

  (pass-if "non-empty, implicit infix"
     (string=? "bla" (string-join '("bla") "|delim|")))

  (pass-if "two strings, implicit infix"
     (string=? "bla|delim|fasel" (string-join '("bla" "fasel") "|delim|")))

  (pass-if "empty, explicit infix"
     (string=? "" (string-join '("") "|delim|" 'infix)))

  (pass-if "empty list, explicit infix"
     (string=? "" (string-join '() "|delim|" 'infix)))

  (pass-if "non-empty, explicit infix"
     (string=? "bla" (string-join '("bla") "|delim|" 'infix)))

  (pass-if "two strings, explicit infix"
     (string=? "bla|delim|fasel" (string-join '("bla" "fasel") "|delim|"
					      'infix)))

  (pass-if-exception "empty list, strict infix"
     exception:strict-infix-grammar
     (string-join '() "|delim|" 'strict-infix))

  (pass-if "empty, strict infix"
     (string=? "" (string-join '("") "|delim|" 'strict-infix)))

  (pass-if "non-empty, strict infix"
     (string=? "foo" (string-join '("foo") "|delim|" 'strict-infix)))

  (pass-if "two strings, strict infix"
     (string=? "foo|delim|bar" (string-join '("foo" "bar") "|delim|"
					    'strict-infix)))

  (pass-if "empty list, prefix"
     (string=? "" (string-join '() "|delim|" 'prefix)))

  (pass-if "empty, prefix"
     (string=? "|delim|" (string-join '("") "|delim|" 'prefix)))

  (pass-if "non-empty, prefix"
     (string=? "|delim|foo" (string-join '("foo") "|delim|" 'prefix)))

  (pass-if "two strings, prefix"
     (string=? "|delim|foo|delim|bar" (string-join '("foo" "bar") "|delim|"
						   'prefix)))

  (pass-if "empty list, suffix"
     (string=? "" (string-join '() "|delim|" 'suffix)))

  (pass-if "empty, suffix"
     (string=? "|delim|" (string-join '("") "|delim|" 'suffix)))

  (pass-if "non-empty, suffix"
     (string=? "foo|delim|" (string-join '("foo") "|delim|" 'suffix)))

  (pass-if "two strings, suffix"
     (string=? "foo|delim|bar|delim|" (string-join '("foo" "bar") "|delim|"
						   'suffix))))

(with-test-prefix "string-copy"

  (pass-if "empty string"
    (string=? "" (string-copy "")))

  (pass-if "full string"
    (string=? "foo-bar" (string-copy "foo-bar")))

  (pass-if "start index"
    (string=? "o-bar" (string-copy "foo-bar" 2)))

  (pass-if "start and end index"
    (string=? "o-ba" (string-copy "foo-bar" 2 6)))
)

(with-test-prefix "substring/shared"

  (pass-if "empty string"
    (eq? "" (substring/shared "" 0)))

  (pass-if "non-empty string"
    (string=? "foo" (substring/shared "foo-bar" 0 3)))

  (pass-if "non-empty string, not eq?"
    (string=? "foo-bar" (substring/shared "foo-bar" 0 7))))

(with-test-prefix "string-copy!"

  (pass-if "non-empty string"
    (string=? "welld, oh yeah!"
	      (let* ((s "hello")
		     (t (string-copy "world, oh yeah!")))
		(string-copy! t 1 s 1 3)
		t))))

(with-test-prefix "string-take"

  (pass-if "empty string"
    (string=? "" (string-take "foo bar braz" 0)))

  (pass-if "non-empty string"
    (string=? "foo " (string-take "foo bar braz" 4)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-take "foo bar braz" 12))))

(with-test-prefix "string-take-right"

  (pass-if "empty string"
    (string=? "" (string-take-right "foo bar braz" 0)))

  (pass-if "non-empty string"
    (string=? "braz" (string-take-right "foo bar braz" 4)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-take-right "foo bar braz" 12))))

(with-test-prefix "string-drop"

  (pass-if "empty string"
    (string=? "" (string-drop "foo bar braz" 12)))

  (pass-if "non-empty string"
    (string=? "braz" (string-drop "foo bar braz" 8)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-drop "foo bar braz" 0))))

(with-test-prefix "string-drop-right"

  (pass-if "empty string"
    (string=? "" (string-drop-right "foo bar braz" 12)))

  (pass-if "non-empty string"
    (string=? "foo " (string-drop-right "foo bar braz" 8)))

  (pass-if "full string"
    (string=? "foo bar braz" (string-drop-right "foo bar braz" 0))))

(with-test-prefix "string-pad"

  (pass-if "empty string, zero pad"
    (string=? "" (string-pad "" 0)))

  (pass-if "empty string, zero pad, pad char"
    (string=? "" (string-pad "" 0)))

  (pass-if "empty pad string, 2 pad "
    (string=? "  " (string-pad "" 2)))

  (pass-if "empty pad string, 2 pad, pad char"
    (string=? "!!" (string-pad "" 2 #\!)))

  (pass-if "empty pad string, 2 pad, pad char, start index"
    (string=? "!c" (string-pad "abc" 2 #\! 2)))

  (pass-if "empty pad string, 2 pad, pad char, start and end index"
    (string=? "!c" (string-pad "abcd" 2 #\! 2 3)))

  (pass-if "freestyle 1"
    (string=? "32" (string-pad (number->string 532) 2 #\!)))

  (pass-if "freestyle 2"
    (string=? "!532" (string-pad (number->string 532) 4 #\!))))

(with-test-prefix "string-pad-right"

  (pass-if "empty string, zero pad"
    (string=? "" (string-pad-right "" 0)))

  (pass-if "empty string, zero pad, pad char"
    (string=? "" (string-pad-right "" 0)))

  (pass-if "empty pad string, 2 pad "
    (string=? "  " (string-pad-right "" 2)))

  (pass-if "empty pad string, 2 pad, pad char"
    (string=? "!!" (string-pad-right "" 2 #\!)))

  (pass-if "empty pad string, 2 pad, pad char, start index"
    (string=? "c!" (string-pad-right "abc" 2 #\! 2)))

  (pass-if "empty pad string, 2 pad, pad char, start and end index"
    (string=? "c!" (string-pad-right "abcd" 2 #\! 2 3)))

  (pass-if "freestyle 1"
    (string=? "53" (string-pad-right (number->string 532) 2 #\!)))

  (pass-if "freestyle 2"
    (string=? "532!" (string-pad-right (number->string 532) 4 #\!))))

(with-test-prefix "string-trim"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-trim "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-trim "abcde" "zzz")))

  (pass-if "empty string"
    (string=? "" (string-trim "")))

  (pass-if "no char/pred"
    (string=? "foo " (string-trim " \tfoo ")))

  (pass-if "start index, pred"
    (string=? "foo " (string-trim " \tfoo " char-whitespace? 1)))

  (pass-if "start and end index, pred"
    (string=? "f" (string-trim " \tfoo " char-whitespace? 1 3)))

  (pass-if "start index, char"
    (string=? "\tfoo " (string-trim " \tfoo " #\space 1)))

  (pass-if "start and end index, char"
    (string=? "\tf" (string-trim " \tfoo " #\space 1 3)))

  (pass-if "start index, charset"
    (string=? "foo " (string-trim " \tfoo " char-set:whitespace 1)))

  (pass-if "start and end index, charset"
    (string=? "f" (string-trim " \tfoo " char-set:whitespace 1 3))))

(with-test-prefix "string-trim-right"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-trim-right "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-trim-right "abcde" "zzz")))

  (pass-if "empty string"
    (string=? "" (string-trim-right "")))

  (pass-if "no char/pred"
    (string=? " \tfoo" (string-trim-right " \tfoo ")))

  (pass-if "start index, pred"
    (string=? "\tfoo" (string-trim-right " \tfoo " char-whitespace? 1)))

  (pass-if "start and end index, pred"
    (string=? "\tf" (string-trim-right " \tfoo " char-whitespace? 1 3)))

  (pass-if "start index, char"
    (string=? "\tfoo" (string-trim-right " \tfoo " #\space 1)))

  (pass-if "start and end index, char"
    (string=? "\tf" (string-trim-right " \tfoo " #\space 1 3)))

  (pass-if "start index, charset"
    (string=? "\tfoo" (string-trim-right " \tfoo " char-set:whitespace 1)))

  (pass-if "start and end index, charset"
    (string=? "\tf" (string-trim-right " \tfoo " char-set:whitespace 1 3))))

(with-test-prefix "string-trim-both"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-trim-both "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-trim-both "abcde" "zzz")))

  (pass-if "empty string"
    (string=? "" (string-trim-both "")))

  (pass-if "no char/pred"
    (string=? "foo" (string-trim-both " \tfoo ")))

  (pass-if "start index, pred"
    (string=? "foo" (string-trim-both " \tfoo " char-whitespace? 1)))

  (pass-if "start and end index, pred"
    (string=? "f" (string-trim-both " \tfoo " char-whitespace? 1 3)))

  (pass-if "start index, char"
    (string=? "\tfoo" (string-trim-both " \tfoo " #\space 1)))

  (pass-if "start and end index, char"
    (string=? "\tf" (string-trim-both " \tfoo " #\space 1 3)))

  (pass-if "start index, charset"
    (string=? "foo" (string-trim-both " \tfoo " char-set:whitespace 1)))

  (pass-if "start and end index, charset"
    (string=? "f" (string-trim-both " \tfoo " char-set:whitespace 1 3))))

(define s0 (make-string 200 #\!))
(define s1 (make-string 0 #\!))

(with-test-prefix "string-fill!"

  (pass-if "empty string, no indices"
    (string-fill! s1 #\*)
    (= (string-length s1) 0))

  (pass-if "empty string, start index"
    (string-fill! s1 #\* 0)
    (= (string-length s1) 0))

  (pass-if "empty string, start and end index"
    (string-fill! s1 #\* 0 0)
    (= (string-length s1) 0))

  (pass-if "no indices"
    (string-fill! s0 #\*)
    (char=? (string-ref s0 0) #\*))

  (pass-if "start index"
    (string-fill! s0 #\+ 10)
    (char=? (string-ref s0 11) #\+))

  (pass-if "start and end index"
    (string-fill! s0 #\| 12 20)
    (char=? (string-ref s0 13) #\|)))

(with-test-prefix "string-prefix-length"

  (pass-if "empty prefix"
    (= 0 (string-prefix-length "" "foo bar")))

  (pass-if "non-empty prefix - match"
    (= 3 (string-prefix-length "foo" "foo bar")))

  (pass-if "non-empty prefix - no match"
    (= 0 (string-prefix-length "bar" "foo bar"))))

(with-test-prefix "string-prefix-length-ci"

  (pass-if "empty prefix"
    (= 0 (string-prefix-length-ci "" "foo bar")))

  (pass-if "non-empty prefix - match"
    (= 3 (string-prefix-length-ci "fOo" "foo bar")))

  (pass-if "non-empty prefix - no match"
    (= 0 (string-prefix-length-ci "bAr" "foo bar"))))

(with-test-prefix "string-suffix-length"

  (pass-if "empty suffix"
    (= 0 (string-suffix-length "" "foo bar")))

  (pass-if "non-empty suffix - match"
    (= 3 (string-suffix-length "bar" "foo bar")))

  (pass-if "non-empty suffix - no match"
    (= 0 (string-suffix-length "foo" "foo bar"))))

(with-test-prefix "string-suffix-length-ci"

  (pass-if "empty suffix"
    (= 0 (string-suffix-length-ci "" "foo bar")))

  (pass-if "non-empty suffix - match"
    (= 3 (string-suffix-length-ci "bAr" "foo bar")))

  (pass-if "non-empty suffix - no match"
    (= 0 (string-suffix-length-ci "fOo" "foo bar"))))

(with-test-prefix "string-prefix?"

  (pass-if "empty prefix"
    (string-prefix? "" "foo bar"))

  (pass-if "non-empty prefix - match"
    (string-prefix? "foo" "foo bar"))

  (pass-if "non-empty prefix - no match"
    (not (string-prefix? "bar" "foo bar"))))

(with-test-prefix "string-prefix-ci?"

  (pass-if "empty prefix"
    (string-prefix-ci? "" "foo bar"))

  (pass-if "non-empty prefix - match"
    (string-prefix-ci? "fOo" "foo bar"))

  (pass-if "non-empty prefix - no match"
    (not (string-prefix-ci? "bAr" "foo bar"))))

(with-test-prefix "string-suffix?"

  (pass-if "empty suffix"
    (string-suffix? "" "foo bar"))

  (pass-if "non-empty suffix - match"
    (string-suffix? "bar" "foo bar"))

  (pass-if "non-empty suffix - no match"
    (not (string-suffix? "foo" "foo bar"))))

(with-test-prefix "string-suffix-ci?"

  (pass-if "empty suffix"
    (string-suffix-ci? "" "foo bar"))

  (pass-if "non-empty suffix - match"
    (string-suffix-ci? "bAr" "foo bar"))

  (pass-if "non-empty suffix - no match"
    (not (string-suffix-ci? "fOo" "foo bar"))))

(with-test-prefix "string-index"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-index "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-index "abcde" "zzz")))

  (pass-if "empty string - char"
    (not (string-index "" #\a)))

  (pass-if "non-empty - char - match"
    (= 5 (string-index "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (not (string-index "frobnicate" #\x)))

  (pass-if "empty string - char - start index"
    (not (string-index "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 5 (string-index "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (not (string-index "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-index "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 5 (string-index "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (not (string-index "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-index "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 0 (string-index "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (not (string-index "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-index "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 1 (string-index "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (not (string-index "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-index "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 1 (string-index "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (not (string-index "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-index "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 0 (string-index "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (not (string-index "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-index "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 1 (string-index "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (not (string-index "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-index "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 1 (string-index "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (not (string-index "frobnicate" char-numeric? 2 5)))

  ;; in guile 1.6.7 and earlier this resulted in a segv, because
  ;; SCM_MAKE_CHAR didn't cope with "signed char" arguments containing an
  ;; 8-bit value
  (pass-if "8-bit char in string"
    (begin
      (string-index (string (integer->char 200)) char-numeric?)
      #t)))

(with-test-prefix "string-index-right"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-index-right "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-index-right "abcde" "zzz")))

  (pass-if "empty string - char"
    (not (string-index-right "" #\a)))

  (pass-if "non-empty - char - match"
    (= 5 (string-index-right "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (not (string-index-right "frobnicate" #\x)))

  (pass-if "empty string - char - start index-right"
    (not (string-index-right "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 5 (string-index-right "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (not (string-index-right "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-index-right "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 5 (string-index-right "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (not (string-index-right "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-index-right "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 6 (string-index-right "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (not (string-index-right "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-index-right "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 6 (string-index-right "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (not (string-index-right "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-index-right "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 5 (string-index-right "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (not (string-index-right "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-index-right "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 6 (string-index-right "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (not (string-index-right "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-index-right "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 6 (string-index-right "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (not (string-index-right "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-index-right "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 5 (string-index-right "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (not (string-index-right "frobnicate" char-numeric? 2 5))))

(with-test-prefix "string-skip"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-skip "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-skip "abcde" "zzz")))

  (pass-if "empty string - char"
    (not (string-skip "" #\a)))

  (pass-if "non-empty - char - match"
    (= 0 (string-skip "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (= 0 (string-skip "frobnicate" #\x)))

  (pass-if "empty string - char - start index"
    (not (string-skip "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 1 (string-skip "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (= 2 (string-skip "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-skip "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 1 (string-skip "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (= 2 (string-skip "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-skip "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 3 (string-skip "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (= 0 (string-skip "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-skip "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 3 (string-skip "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (= 2 (string-skip "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-skip "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 3 (string-skip "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (= 2 (string-skip "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-skip "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 3 (string-skip "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (= 0 (string-skip "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-skip "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 3 (string-skip "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (= 2 (string-skip "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-skip "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 3 (string-skip "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (= 2 (string-skip "frobnicate" char-numeric? 2 5))))

(with-test-prefix "string-skip-right"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-skip-right "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-skip-right "abcde" "zzz")))

  (pass-if "empty string - char"
    (not (string-skip-right "" #\a)))

  (pass-if "non-empty - char - match"
    (= 6 (string-skip-right "foo bar" #\a)))

  (pass-if "non-empty - char - no match"
    (= 9 (string-skip-right "frobnicate" #\x)))

  (pass-if "empty string - char - start index-right"
    (not (string-skip-right "" #\a 0)))

  (pass-if "non-empty - char - match - start index"
    (= 6 (string-skip-right "foo bar" #\a 1)))

  (pass-if "non-empty - char - no match - start index"
    (= 9 (string-skip-right "frobnicate" #\x 2)))

  (pass-if "empty string - char - start and end index"
    (not (string-skip-right "" #\a 0 0)))

  (pass-if "non-empty - char - match - start and end index"
    (= 4 (string-skip-right "foo bar" #\a 1 6)))

  (pass-if "non-empty - char - no match - start and end index"
    (= 4 (string-skip-right "frobnicate" #\a 2 5)))

  (pass-if "empty string - charset"
    (not (string-skip-right "" char-set:letter)))

  (pass-if "non-empty - charset - match"
    (= 3 (string-skip-right "foo bar" char-set:letter)))

  (pass-if "non-empty - charset - no match"
    (= 9 (string-skip-right "frobnicate" char-set:digit)))

  (pass-if "empty string - charset - start index"
    (not (string-skip-right "" char-set:letter 0)))

  (pass-if "non-empty - charset - match - start index"
    (= 3 (string-skip-right "foo bar" char-set:letter 1)))

  (pass-if "non-empty - charset - no match - start index"
    (= 9 (string-skip-right "frobnicate" char-set:digit 2)))

  (pass-if "empty string - charset - start and end index"
    (not (string-skip-right "" char-set:letter 0 0)))

  (pass-if "non-empty - charset - match - start and end index"
    (= 3 (string-skip-right "foo bar" char-set:letter 1 6)))

  (pass-if "non-empty - charset - no match - start and end index"
    (= 4 (string-skip-right "frobnicate" char-set:digit 2 5)))

  (pass-if "empty string - pred"
    (not (string-skip-right "" char-alphabetic?)))

  (pass-if "non-empty - pred - match"
    (= 3 (string-skip-right "foo bar" char-alphabetic?)))

  (pass-if "non-empty - pred - no match"
    (= 9 (string-skip-right "frobnicate" char-numeric?)))

  (pass-if "empty string - pred - start index"
    (not (string-skip-right "" char-alphabetic? 0)))

  (pass-if "non-empty - pred - match - start index"
    (= 3 (string-skip-right "foo bar" char-alphabetic? 1)))

  (pass-if "non-empty - pred - no match - start index"
    (= 9 (string-skip-right "frobnicate" char-numeric? 2)))

  (pass-if "empty string - pred - start and end index"
    (not (string-skip-right "" char-alphabetic? 0 0)))

  (pass-if "non-empty - pred - match - start and end index"
    (= 3 (string-skip-right "foo bar" char-alphabetic? 1 6)))

  (pass-if "non-empty - pred - no match - start and end index"
    (= 4 (string-skip-right "frobnicate" char-numeric? 2 5))))

;;
;; string-count
;;

(with-test-prefix "string-count"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-count "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-count "abcde" "zzz")))

  (with-test-prefix "char"

    (pass-if (eqv? 0 (string-count "" #\a)))
    (pass-if (eqv? 0 (string-count "-" #\a)))
    (pass-if (eqv? 1 (string-count "a" #\a)))
    (pass-if (eqv? 0 (string-count "--" #\a)))
    (pass-if (eqv? 1 (string-count "a-" #\a)))
    (pass-if (eqv? 1 (string-count "-a" #\a)))
    (pass-if (eqv? 2 (string-count "aa" #\a)))
    (pass-if (eqv? 0 (string-count "---" #\a)))
    (pass-if (eqv? 1 (string-count "-a-" #\a)))
    (pass-if (eqv? 1 (string-count "a--" #\a)))
    (pass-if (eqv? 2 (string-count "aa-" #\a)))
    (pass-if (eqv? 2 (string-count "a-a" #\a)))
    (pass-if (eqv? 3 (string-count "aaa" #\a)))
    (pass-if (eqv? 1 (string-count "--a" #\a)))
    (pass-if (eqv? 2 (string-count "-aa" #\a))))

  (with-test-prefix "charset"

    (pass-if (eqv? 0 (string-count "" char-set:letter)))
    (pass-if (eqv? 0 (string-count "-" char-set:letter)))
    (pass-if (eqv? 1 (string-count "a" char-set:letter)))
    (pass-if (eqv? 0 (string-count "--" char-set:letter)))
    (pass-if (eqv? 1 (string-count "a-" char-set:letter)))
    (pass-if (eqv? 1 (string-count "-a" char-set:letter)))
    (pass-if (eqv? 2 (string-count "aa" char-set:letter)))
    (pass-if (eqv? 0 (string-count "---" char-set:letter)))
    (pass-if (eqv? 1 (string-count "-a-" char-set:letter)))
    (pass-if (eqv? 1 (string-count "a--" char-set:letter)))
    (pass-if (eqv? 2 (string-count "aa-" char-set:letter)))
    (pass-if (eqv? 2 (string-count "a-a" char-set:letter)))
    (pass-if (eqv? 3 (string-count "aaa" char-set:letter)))
    (pass-if (eqv? 1 (string-count "--a" char-set:letter)))
    (pass-if (eqv? 2 (string-count "-aa" char-set:letter))))

  (with-test-prefix "proc"

    (pass-if (eqv? 0 (string-count "" char-alphabetic?)))
    (pass-if (eqv? 0 (string-count "-" char-alphabetic?)))
    (pass-if (eqv? 1 (string-count "a" char-alphabetic?)))
    (pass-if (eqv? 0 (string-count "--" char-alphabetic?)))
    (pass-if (eqv? 1 (string-count "a-" char-alphabetic?)))
    (pass-if (eqv? 1 (string-count "-a" char-alphabetic?)))
    (pass-if (eqv? 2 (string-count "aa" char-alphabetic?)))
    (pass-if (eqv? 0 (string-count "---" char-alphabetic?)))
    (pass-if (eqv? 1 (string-count "-a-" char-alphabetic?)))
    (pass-if (eqv? 1 (string-count "a--" char-alphabetic?)))
    (pass-if (eqv? 2 (string-count "aa-" char-alphabetic?)))
    (pass-if (eqv? 2 (string-count "a-a" char-alphabetic?)))
    (pass-if (eqv? 3 (string-count "aaa" char-alphabetic?)))
    (pass-if (eqv? 1 (string-count "--a" char-alphabetic?)))
    (pass-if (eqv? 2 (string-count "-aa" char-alphabetic?)))))


(with-test-prefix "string-replace"

  (pass-if "empty string(s), no indices"
    (string=? "" (string-replace "" "")))

  (pass-if "empty string(s), 1 index"
    (string=? "" (string-replace "" "" 0)))

  (pass-if "empty string(s), 2 indices"
    (string=? "" (string-replace "" "" 0 0)))

  (pass-if "empty string(s), 3 indices"
    (string=? "" (string-replace "" "" 0 0 0)))

  (pass-if "empty string(s), 4 indices"
    (string=? "" (string-replace "" "" 0 0 0 0)))

  (pass-if "no indices"
    (string=? "uu" (string-replace "foo bar" "uu")))

  (pass-if "one index"
    (string=? "fuu" (string-replace "foo bar" "uu" 1)))

  (pass-if "two indices"
    (string=? "fuuar" (string-replace "foo bar" "uu" 1 5)))

  (pass-if "three indices"
    (string=? "fuar" (string-replace "foo bar" "uu" 1 5 1)))

  (pass-if "four indices"
    (string=? "fuar" (string-replace "foo bar" "uu" 1 5 1 2))))

(with-test-prefix "string-tokenize"

  (pass-if "empty string, no char/pred"
    (zero? (length (string-tokenize ""))))

  (pass-if "empty string, charset"
    (zero? (length (string-tokenize "" char-set:punctuation))))

  (pass-if "no char/pred"
    (equal? '("foo" "bar" "!a") (string-tokenize "foo\tbar !a")))

  (pass-if "charset"
    (equal? '("foo" "bar" "!a") (string-tokenize "foo\tbar !a"
						char-set:graphic)))

  (pass-if "charset, start index"
    (equal? '("oo" "bar" "!a") (string-tokenize "foo\tbar !a"
						char-set:graphic 1)))

  (pass-if "charset, start and end index"
    (equal? '("oo" "bar" "!") (string-tokenize "foo\tbar !a"
					       char-set:graphic 1 9))))
;;;
;;; string-filter
;;;

(with-test-prefix "string-filter"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-filter "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-filter "abcde" "zzz")))

  (pass-if "empty string, char"
    (string=? "" (string-filter "" #\.)))

  (pass-if "empty string, charset"
    (string=? "" (string-filter "" char-set:punctuation)))

  (pass-if "empty string, pred"
    (string=? "" (string-filter "" char-alphabetic?)))

  (pass-if "char"
    (string=? "..." (string-filter ".foo.bar." #\.)))

  (pass-if "charset"
    (string=? "..." (string-filter ".foo.bar." char-set:punctuation)))

  (pass-if "pred"
    (string=? "foobar" (string-filter ".foo.bar." char-alphabetic?)))

  (pass-if "char, start index"
    (string=? ".." (string-filter ".foo.bar." #\. 2)))

  (pass-if "charset, start index"
    (string=? ".." (string-filter ".foo.bar." char-set:punctuation 2)))

  (pass-if "pred, start index"
    (string=? "oobar" (string-filter ".foo.bar." char-alphabetic? 2)))

  (pass-if "char, start and end index"
    (string=? "" (string-filter ".foo.bar." #\. 2 4)))

  (pass-if "charset, start and end index"
    (string=? "" (string-filter ".foo.bar." char-set:punctuation 2 4)))

  (pass-if "pred, start and end index"
    (string=? "oo" (string-filter ".foo.bar." char-alphabetic? 2 4)))

  (with-test-prefix "char"

    (pass-if (equal? "x" (string-filter "x" #\x)))
    (pass-if (equal? "xx" (string-filter "xx" #\x)))
    (pass-if (equal? "xx" (string-filter "xyx" #\x)))
    (pass-if (equal? "x" (string-filter "xyyy" #\x)))
    (pass-if (equal? "x" (string-filter "yyyx" #\x)))

    (pass-if (equal? "xx" (string-filter "xxx" #\x 1)))
    (pass-if (equal? "xx" (string-filter "xxx" #\x 0 2)))
    (pass-if (equal? "x" (string-filter "xyx" #\x 1)))
    (pass-if (equal? "x" (string-filter "yxx" #\x 0 2)))

    ;; leading and trailing removals
    (pass-if (string=? "" (string-filter "." #\x)))
    (pass-if (string=? "" (string-filter ".." #\x)))
    (pass-if (string=? "" (string-filter "..." #\x)))
    (pass-if (string=? "x" (string-filter ".x" #\x)))
    (pass-if (string=? "x" (string-filter "..x" #\x)))
    (pass-if (string=? "x" (string-filter "...x" #\x)))
    (pass-if (string=? "x" (string-filter "x." #\x)))
    (pass-if (string=? "x" (string-filter "x.." #\x)))
    (pass-if (string=? "x" (string-filter "x..." #\x)))
    (pass-if (string=? "x" (string-filter "...x..." #\x))))

  (with-test-prefix "charset"

    (let ((charset (char-set #\x #\y)))
      (pass-if (equal? "x" (string-filter "x" charset)))
      (pass-if (equal? "xx" (string-filter "xx" charset)))
      (pass-if (equal? "xy" (string-filter "xy" charset)))
      (pass-if (equal? "x" (string-filter "xaaa" charset)))
      (pass-if (equal? "y" (string-filter "aaay" charset)))

      (pass-if (equal? "yx" (string-filter "xyx" charset 1)))
      (pass-if (equal? "xy" (string-filter "xyx" charset 0 2)))
      (pass-if (equal? "x" (string-filter "xax" charset 1)))
      (pass-if (equal? "x" (string-filter "axx" charset 0 2))))

    ;; leading and trailing removals
    (pass-if (string=? "" (string-filter "." char-set:letter)))
    (pass-if (string=? "" (string-filter ".." char-set:letter)))
    (pass-if (string=? "" (string-filter "..." char-set:letter)))
    (pass-if (string=? "x" (string-filter ".x" char-set:letter)))
    (pass-if (string=? "x" (string-filter "..x" char-set:letter)))
    (pass-if (string=? "x" (string-filter "...x" char-set:letter)))
    (pass-if (string=? "x" (string-filter "x." char-set:letter)))
    (pass-if (string=? "x" (string-filter "x.." char-set:letter)))
    (pass-if (string=? "x" (string-filter "x..." char-set:letter)))
    (pass-if (string=? "x" (string-filter "...x..." char-set:letter)))))

;;;
;;; string-delete
;;;

(with-test-prefix "string-delete"

  (with-test-prefix "bad char_pred"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-delete "abcde" 123))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-delete "abcde" "zzz")))

  (pass-if "empty string, char"
    (string=? "" (string-delete "" #\.)))

  (pass-if "empty string, charset"
    (string=? "" (string-delete "" char-set:punctuation)))

  (pass-if "empty string, pred"
    (string=? "" (string-delete "" char-alphabetic?)))

  (pass-if "char"
    (string=? "foobar" (string-delete ".foo.bar." #\.)))

  (pass-if "charset"
    (string=? "foobar" (string-delete ".foo.bar." char-set:punctuation)))

  (pass-if "pred"
    (string=? "..." (string-delete ".foo.bar." char-alphabetic?)))

  (pass-if "char, start index"
    (string=? "oobar" (string-delete ".foo.bar." #\. 2)))

  (pass-if "charset, start index"
    (string=? "oobar" (string-delete ".foo.bar." char-set:punctuation 2)))

  (pass-if "pred, start index"
    (string=? ".." (string-delete ".foo.bar." char-alphabetic? 2)))

  (pass-if "char, start and end index"
    (string=? "oo" (string-delete ".foo.bar." #\. 2 4)))

  (pass-if "charset, start and end index"
    (string=? "oo" (string-delete ".foo.bar." char-set:punctuation 2 4)))

  (pass-if "pred, start and end index"
    (string=? "" (string-delete ".foo.bar." char-alphabetic? 2 4)))

  ;; leading and trailing removals
  (pass-if (string=? "" (string-delete "." #\.)))
  (pass-if (string=? "" (string-delete ".." #\.)))
  (pass-if (string=? "" (string-delete "..." #\.)))
  (pass-if (string=? "x" (string-delete ".x" #\.)))
  (pass-if (string=? "x" (string-delete "..x" #\.)))
  (pass-if (string=? "x" (string-delete "...x" #\.)))
  (pass-if (string=? "x" (string-delete "x." #\.)))
  (pass-if (string=? "x" (string-delete "x.." #\.)))
  (pass-if (string=? "x" (string-delete "x..." #\.)))
  (pass-if (string=? "x" (string-delete "...x..." #\.)))

  ;; leading and trailing removals
  (pass-if (string=? "" (string-delete "." char-set:punctuation)))
  (pass-if (string=? "" (string-delete ".." char-set:punctuation)))
  (pass-if (string=? "" (string-delete "..." char-set:punctuation)))
  (pass-if (string=? "x" (string-delete ".x" char-set:punctuation)))
  (pass-if (string=? "x" (string-delete "..x" char-set:punctuation)))
  (pass-if (string=? "x" (string-delete "...x" char-set:punctuation)))
  (pass-if (string=? "x" (string-delete "x." char-set:punctuation)))
  (pass-if (string=? "x" (string-delete "x.." char-set:punctuation)))
  (pass-if (string=? "x" (string-delete "x..." char-set:punctuation)))
  (pass-if (string=? "x" (string-delete "...x..." char-set:punctuation))))


(with-test-prefix "string-map"

  (with-test-prefix "bad proc"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-map 123 "abcde"))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-map "zzz" "abcde")))

  (pass-if "constant"
    (string=? "xxx" (string-map (lambda (c) #\x) "foo")))

  (pass-if "identity"
    (string=? "foo" (string-map identity "foo")))

  (pass-if "upcase"
    (string=? "FOO" (string-map char-upcase "foo"))))

(with-test-prefix "string-map!"

  (with-test-prefix "bad proc"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-map 123 "abcde"))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-map "zzz" "abcde")))

  (pass-if "constant"
    (let ((str (string-copy "foo")))
      (string-map! (lambda (c) #\x) str)
      (string=? str "xxx")))

  (pass-if "identity"
    (let ((str (string-copy "foo")))
      (string-map! identity str)
      (string=? str "foo")))

  (pass-if "upcase"
    (let ((str (string-copy "foo")))
      (string-map! char-upcase str)
      (string=? str "FOO"))))

(with-test-prefix "string-for-each"

  (with-test-prefix "bad proc"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-for-each 123 "abcde"))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-for-each "zzz" "abcde")))

  (pass-if "copy"
     (let* ((foo "foo")
            (bar (make-string (string-length foo)))
            (i 0))
       (string-for-each
        (lambda (c) (string-set! bar i c) (set! i (1+ i))) foo)
       (string=? foo bar))))

(with-test-prefix "string-for-each-index"

  (with-test-prefix "bad proc"

    (pass-if-exception "integer" exception:wrong-type-arg
      (string-for-each-index 123 "abcde"))

    (pass-if-exception "string" exception:wrong-type-arg
      (string-for-each-index "zzz" "abcde")))

  (pass-if "index"
     (let* ((foo "foo")
            (bar (make-string (string-length foo))))
       (string-for-each-index
        (lambda (i) (string-set! bar i (string-ref foo i))) foo)
       (string=? foo bar))))

