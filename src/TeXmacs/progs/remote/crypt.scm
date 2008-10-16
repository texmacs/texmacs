
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

(texmacs-module (remote crypt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further utilities for files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (with-temp-file name s . body)
  `(let* ((tmp-u (url-temp))
	  (,name (escape-shell (url-concretize tmp-u))))
     (string-save ,s tmp-u)
     (with r (begin ,@body)
       (system-remove tmp-u)
       r)))

(tm-define (system* . args)
  (system (apply string-append args)))

(tm-define (eval-system* . args)
  (eval-system (apply string-append args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion to and from base 64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string->base64 s)
  (with-temp-file message s
    (string-replace (eval-system* "openssl enc -base64 -in " message)
		    "\n" "#")))

(tm-define (base64->string s)
  (with-temp-file message (string-replace s "#" "\n")
    (eval-system* "openssl enc -d -base64 -in " message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crypting with RSA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (rsa-generate)
  (eval-system* "openssl genrsa 2048 2> /dev/null"))

(tm-define (rsa-private->public private-key)
  (with-temp-file key private-key
    (eval-system* "openssl rsa -in " key " -pubout 2> /dev/null")))

(tm-define (rsa-encode what public-key)
  (with-temp-file msg what
    (with-temp-file key public-key
      (eval-system* "openssl rsautl -in " msg
		    " -pubin -inkey " key " -encrypt"))))

(tm-define (rsa-decode what private-key)
  (with-temp-file msg what
    (with-temp-file key private-key
      (eval-system* "openssl rsautl -in " msg
		    " -inkey " key " -decrypt"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crypting with DES3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (secret-generate . len)
  (with l (if (null? len) 32 (car len))
    (eval-system* "openssl rand -base64 " (number->string l))))

(tm-define (secret-encode what secret-key)
  (with-temp-file msg what
    (with-temp-file key secret-key
      (eval-system* "openssl aes-256-cbc -nosalt -in " msg
		    " -pass file:" key))))

(tm-define (secret-decode what secret-key)
  (with-temp-file msg what
    (with-temp-file key secret-key
      (eval-system* "openssl aes-256-cbc -nosalt -d -in " msg
		    " -pass file:" key))))

(tm-define (secret-hash password)
  (with-temp-file pass password
    (secret-encode "TeXmacs worgelt BlauwBilGorgels" password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prevent third persons to pretend being one of the communicants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (add-verification msg)
  (string-append "tm:" msg))

(tm-define (remove-verification msg)
  (and (string? msg)
       (>= (string-length msg) 3)
       (== (substring msg 0 3) "tm:")
       (substring msg 3 (string-length msg))))
