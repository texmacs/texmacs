
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : xmltm-test.scm
;; DESCRIPTION : Test suite for converter environments.
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools environment-test)
  (:use (convert tools environment))
  (:export regtest-environment))

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
   (test "never bound"
	 (error-or-value
	  (lambda ()
	    (let ((env (environment)))
	      (environment-ref env foo))))
	 'texmacs-error)
   (test "unbound"
	 (error-or-value
	  (lambda ()
	    (let ((env (environment)))
	      (with-environment
	       env ((foo "bar"))
	       (noop))
	      (environment-ref env foo))))
	 'texmacs-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-environment)
  (let ((n (+ (regtest-environment-base))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of environment: ok\n")))
