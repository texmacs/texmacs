
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : crypt.scm
;; DESCRIPTION : basic routines for cryptography
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tools crypt))
(use-modules (tools base) (tools abbrevs) (tools file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion to and from base 64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-string-replace s what by)
  (let* ((By (list->string (list by)))
	 (l1 (string-split s what))
	 (l2 (map (lambda (x) (list By x)) l1))
	 (l3 (cdr (apply append l2))))
    (apply string-append l3)))

(define-public (string->base64 s)
  (with r (with-temp-file mess s
	    (eval-system* "openssl enc -base64 -in " mess))
    (my-string-replace r #\newline #\#)))

(define-public (base64->string s)
  (with r (my-string-replace s #\# #\newline)
    (with-temp-file mess r
      (eval-system* "openssl enc -d -base64 -in " mess))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crypting with RSA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (rsa-generate)
  (eval-system* "openssl genrsa 2048 2> /dev/null"))

(define-public (rsa-private->public private-key)
  (with-temp-file key private-key
    (eval-system* "openssl rsa -in " key " -pubout 2> /dev/null")))

(define-public (rsa-encode what public-key)
  (with-temp-file msg what
    (with-temp-file key public-key
      (eval-system* "openssl rsautl -in " msg
		    " -pubin -inkey " key " -encrypt"))))

(define-public (rsa-decode what private-key)
  (with-temp-file msg what
    (with-temp-file key private-key
      (eval-system* "openssl rsautl -in " msg
		    " -inkey " key " -decrypt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crypting with DES3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (secret-generate)
  (eval-system* "openssl rand -base64 32"))

(define-public (secret-encode what secret-key)
  (with-temp-file msg what
    (with-temp-file key secret-key
      (eval-system* "openssl aes-256-cbc -nosalt -in " msg
		    " -pass file:" key))))

(define-public (secret-decode what secret-key)
  (with-temp-file msg what
    (with-temp-file key secret-key
      (eval-system* "openssl aes-256-cbc -nosalt -d -in " msg
		    " -pass file:" key))))

(define-public (secret-hash password)
  (with-temp-file pass password
    (secret-encode "TeXmacs worgelt BlauwBilGorgels" password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prevent third persons to pretend being one of the communicants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (add-verification msg)
  (string-append "tm:" msg))

(define-public (remove-verification msg)
  (and (string? msg)
       (>= (string-length msg) 3)
       (== (substring msg 0 3) "tm:")
       (substring msg 3 (string-length msg))))
