
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gpg-wallet.scm
;; DESCRIPTION : gpg based wallet
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils gpg gpg-wallet)
  (:use (utils gpg gpg-base)))

;(display "TeXmacs] Loading GnuPG wallet\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings and initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpg-wallet-url
  (url-concretize "$TEXMACS_HOME_PATH/system/gnupg/wallet"))

(define gpg-wallet-table-url
  (url-append gpg-wallet-url "table.gpg"))

(define gpg-wallet-table "") ; prevents from saving empty tables

(define gpg-wallet-key-fingerprint "")

(define (notify-gpg-wallet-key-fingerprint var val)
  (set! gpg-wallet-key-fingerprint val))

(define-preferences
  ("gpg wallet key fingerprint" "" notify-gpg-wallet-key-fingerprint))

(define-public (gpg-wallet-on?)
  (not (string? gpg-wallet-table)))

(define-public (gpg-wallet-initialized?)
  (and (supports-gpg?)
  (if (not (url-exists? gpg-wallet-table-url))
    #f
    (with fingerprints (gpg-secret-key-fingerprints gpg-wallet-url)
      (and fingerprints (not (null? fingerprints))
	   (string= (car fingerprints) gpg-wallet-key-fingerprint))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gpg-wallet-correct-passphrase? passphrase)
  (and (gpg-wallet-initialized?)
       (gpg-correct-passphrase? gpg-wallet-key-fingerprint
				passphrase gpg-wallet-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on/off wallet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gpg-wallet-turn-on passphrase)
  (if (gpg-wallet-on?) #t
    (if (not (gpg-wallet-initialized?)) #f
      (with b (gpg-load-decrypt-object gpg-wallet-table-url
 	  			       passphrase gpg-wallet-url)
      (if b
        (begin (set! gpg-wallet-table (list->ahash-table b))
               #t)
        #f)))))

(define-public (gpg-wallet-save)
  (and (gpg-wallet-on?)
    (gpg-encrypt-save-object gpg-wallet-table-url
      (ahash-table->list gpg-wallet-table)
      gpg-wallet-key-fingerprint gpg-wallet-url)))

(define-public (gpg-wallet-turn-off)
  (if (and (gpg-wallet-on?)
           (gpg-encrypt-save-object gpg-wallet-table-url
	     (ahash-table->list gpg-wallet-table)
	     gpg-wallet-key-fingerprint gpg-wallet-url))
    (begin (set! gpg-wallet-table "") #t)
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gpg-wallet-initialize passphrase)
  ;; very first initialization 
  (when (not (gpg-wallet-on?))
  (when (not (gpg-wallet-initialized?))
  (when (not (url-exists? gpg-wallet-url))
    (system-mkdir gpg-wallet-url)
    (if (not (or (os-mingw?) (os-win32?)))
      (system-1 "chmod og-rwx" gpg-wallet-url)))
  (when (null? (gpg-secret-keys gpg-wallet-url))
    (gpg-gen-key "__TeXmacs_wallet__"
                 "TeXmacs wallet" "" passphrase
                 gpg-wallet-url))
  (with fingerprints (gpg-secret-key-fingerprints gpg-wallet-url)
    (if (not (null? fingerprints))
      (begin (set-preference "gpg wallet key fingerprint" (car fingerprints))
	     (set! gpg-wallet-table (make-ahash-table))
             (gpg-wallet-turn-off))
      #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reinitialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (system-remove-directory url)
  (with l (url->list (url-expand (url-complete
            (url-append url (url-any)) "f")))
  (for-each system-remove l))
  (rmdir (url->system url)))

(define-public (gpg-wallet-reinitialize old-passphrase new-passphrase)
  ;; Regenerate secret key and password with no data loss.
  (if (not (gpg-wallet-initialized?)) #f
  (with wallet-backup (url-glue gpg-wallet-url
    (string-append "-" (number->string (current-time))))
  (with b #t
  (with on? (gpg-wallet-on?)
  (when (not on?)
    (set! b (gpg-wallet-turn-on old-passphrase)))
  (if b
    (begin
      (with table-backup gpg-wallet-table
        (gpg-wallet-turn-off)
        (system-move gpg-wallet-url wallet-backup)
        (if (gpg-wallet-initialize new-passphrase)
	  (begin
	    (gpg-wallet-turn-on new-passphrase)
	    (set! gpg-wallet-table table-backup)
	    (when (not on?) (gpg-wallet-turn-off))
	    (system-remove-directory wallet-backup)
	    #t)
	  (begin
	    (system-move wallet-backup gpg-wallet-url)
	    #f))))
    #f))))))

(define-public (gpg-wallet-destroy)
  ;; Delete the current wallet: for security all data are completely lost!
  (if (not (gpg-wallet-initialized?)) #f
    ;(with wallet-backup (url-glue gpg-wallet-url
    ;  (string-append "-" (number->string (current-time))))
    ;(system-move gpg-wallet-url wallet-backup))
    (begin
      (system-remove-directory gpg-wallet-url)
      (not (url-exists? gpg-wallet-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set, get, delete entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gpg-wallet-set key val)
  (if (gpg-wallet-on?)
    (begin
      (ahash-set! gpg-wallet-table key val)
      (gpg-wallet-save))
    #f))

(define-public (gpg-wallet-get key)
  (if (gpg-wallet-on?)
    (ahash-ref gpg-wallet-table key)
    #f))

(define-public (gpg-wallet-delete key)
  (if (gpg-wallet-on?)
    (begin
      (ahash-remove! gpg-wallet-table key)
      (gpg-wallet-save))
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gpg-wallet-entries)
  (if (gpg-wallet-on?)
    (ahash-table->list gpg-wallet-table)
    #f))
