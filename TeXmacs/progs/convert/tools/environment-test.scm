
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : xmltm-test.scm
;; DESCRIPTION : Test suite for converter environments.
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools environment-test)
  (:use (convert tools environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-environment-base)
  (define (error-or-value thunk)
    (catch 'texmacs-error thunk (lambda (key msg . args) key)))
  (regression-test-group
   "environment basic tools" "env-base"
   :none :none
   (test "simple"
	 (let ((env (environment)))
	   (with-environment
	    env ((foo "bar")
		 (spam (string-append "eg" "gs")))
	    (list (environment-ref env foo) (environment-ref env spam))))
	 '("bar" "eggs"))
   (test "nested"
	 (let ((env (environment)) (out '()))
	   (with-environment
	    env ((foo "bar"))
	    (set-rcons! out (environment-ref env foo))
	    (with-environment
	     env ((foo "baz"))
	     (set-rcons! out (environment-ref env foo)))
	    (set-rcons! out (environment-ref env foo)))
	   out)
	 '("bar" "baz" "bar"))
;   (test "never bound"
;	 (error-or-value
;	  (lambda ()
;	    (let ((env (environment)))
;	      (environment-ref env foo))))
;	 "")
;   (test "unbound"
;	 (error-or-value
;	  (lambda ()
;	    (let ((env (environment)))
;	      (with-environment
;	       env ((foo "bar"))
;	       (noop))
;	      (environment-ref env foo))))
;	 "")
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-environment)
  (let ((n (+ (regtest-environment-base))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of environment: ok\n")))
