;;;; format.test --- test suite for Guile's CL-ish format  -*- scheme -*-
;;;; Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de> --- June 2001
;;;;
;;;; 	Copyright (C) 2001, 2003, 2004, 2006 Free Software Foundation, Inc.
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

(define-module (test-format)
  #:use-module (test-suite lib)
  #:use-module (ice-9 format))


;;; FORMAT Basic Output

(with-test-prefix "format basic output"
  (pass-if "format ~% produces a new line"
	   (string=? (format "~%") "\n"))
  (pass-if "format ~& starts a fresh line"
	   (string=? (format "~&abc~&~&") "abc\n"))
  (pass-if "format ~& is stateless but works properly across outputs via port-column"
	   (string=?
	    (with-output-to-string
	      (lambda ()
		(display "xyz")
		(format #t "~&abc")
		(format #f "~&")	; shall have no effect
		(format #t "~&~&")))
	    "xyz\nabc\n"))
  (pass-if "format ~F (format-out-substr) maintains the column correctly"
	   (= (string-length (format "~@F~20T" 1)) 20)))

;;;
;;; misc
;;;

(with-test-prefix "format"

  ;; in guile 1.6.4 and earlier, excess arguments were an error, but this
  ;; changed to follow the common lisp spec
  (pass-if "excess arguments ignored A"
    (string=? (format #f "" 1 2 3 4) ""))
  (pass-if "excess arguments ignored B"
    (string=? (format #f "~a ~a" 1 2 3 4) "1 2")))

;;;
;;; ~d
;;;

(with-test-prefix "~d decimal integer"

  (with-test-prefix "~@d"

    (pass-if "-1"
      (string=? (format #f "~@d" -1) "-1"))

    ;; in guile 1.6.4 and earlier, ~@d gave "0" but we think "+0" is what the
    ;; common lisp spec intendes
    (pass-if "+0"
      (string=? (format #f "~@d" 0) "+0"))

    (pass-if "+1"
      (string=? (format #f "~@d" 1) "+1"))))

;;;
;;; ~f
;;;

(with-test-prefix "~f fixed-point"

  (pass-if "1.5"
    (string=? "1.5" (format #f "~f" 1.5)))
  
  ;; in guile prior to 1.6.9 and 1.8.1, leading zeros were incorrectly
  ;; stripped, moving the decimal point and giving "25.0" here
  (pass-if "string 02.5"
    (string=? "2.5" (format #f "~f" "02.5"))))

;;;
;;; ~{
;;;

(with-test-prefix "~{ iteration"

  ;; In Guile 1.6.4 and earlier, the maximum iterations parameter defaulted
  ;; to 100, but it's now like Common Lisp where the default is no limit
  (pass-if "no arbitrary iteration limit"
    (= (string-length (format "~{~a~}" (make-list 200 #\b))) 200)))
