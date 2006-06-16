
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
  `(with ,name (url-concretize (url-temp))
     (string-save ,s ,name)
     (with r (begin ,@body)
       (system-remove ,name)
       r)))

(tm-define (system* . args)
  (system (apply string-append args)))

(tm-define (eval-system* . args)
  (eval-system (apply string-append args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion to and from base 64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (base64 x nr)
  (if (== nr 0) '()
      (append (base64 (quotient x 64) (- nr 1))
	      (list (remainder x 64)))))

(define (list->base64 l)
  (cond ((null? l) l)
	((null? (cdr l))
	 (base64 (car l) 2))
	((null? (cddr l))
	 (base64 (+ (* (car l) 256) (cadr l)) 3))
	(else
	 (append (base64 (+ (* (car l) 65536) (* (cadr l) 256) (caddr l)) 4)
		 (list->base64 (cdddr l))))))

(tm-define (string->base64 s)
  (let* ((l1 (string->list s))
	 (l2 (map char->integer l1))
	 (r1 (list->base64 l2))
	 (r2 (map (lambda (i) (+ i 48)) r1))
	 (r3 (map integer->char r2)))
    (list->string r3)))

(define (base256 x nr)
  (if (== nr 0) '()
      (append (base256 (quotient x 256) (- nr 1))
	      (list (remainder x 256)))))

(define (base64->list l)
  (cond ((null? l) l)
	((null? (cdr l)) ;; should never occur
	 (base256 (car l) 1))
	((null? (cddr l))
	 (base256 (+ (* (car l) 64) (cadr l)) 1))
	((null? (cdddr l))
	 (base256 (+ (* (car l) 4096) (* (cadr l) 64) (caddr l)) 2))
	(else
	 (append (base256 (+ (* (car l) 262144) (* (cadr l) 4096)
			     (* (caddr l) 64) (cadddr l)) 3)
		 (base64->list (cddddr l))))))

(tm-define (base64->string s)
  (let* ((l1 (string->list s))
	 (l2 (map char->integer l1))
	 (l3 (map (lambda (i) (- i 48)) l2))
	 (r1 (base64->list l3))
	 (r2 (map integer->char r1)))
    (list->string r2)))

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
