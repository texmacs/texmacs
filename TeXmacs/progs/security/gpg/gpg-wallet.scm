
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

(texmacs-module (security gpg gpg-wallet)
  (:use (security gpg gpg-base)))

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

(tm-define (gpg-wallet-on?)
  (nstring? gpg-wallet-table))

(tm-define (gpg-wallet-initialized?)
  (and (supports-gpg?)
       (url-exists? gpg-wallet-table-url)
       (with fingerprints (gpg-secret-key-fingerprints gpg-wallet-url)
         (and fingerprints (not (null? fingerprints))
              (== (car fingerprints) gpg-wallet-key-fingerprint)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-wallet-correct-passphrase? passphrase)
  (and (gpg-wallet-initialized?)
       (gpg-correct-passphrase? gpg-wallet-key-fingerprint
				passphrase gpg-wallet-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on/off wallet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-wallet-turn-on passphrase)
  (or (gpg-wallet-on?)
      (and (gpg-wallet-initialized?)
           (and-with b (gpg-load-decrypt-object gpg-wallet-table-url
                                                passphrase gpg-wallet-url)
             (set! gpg-wallet-table (list->ahash-table b))
             #t))))

(tm-define (gpg-wallet-save)
  (and (gpg-wallet-on?)
       (gpg-encrypt-save-object gpg-wallet-table-url
                                (ahash-table->list gpg-wallet-table)
                                gpg-wallet-key-fingerprint gpg-wallet-url)))

(tm-define (gpg-wallet-turn-off)
  (and (gpg-wallet-on?)
       (gpg-encrypt-save-object gpg-wallet-table-url
                                (ahash-table->list gpg-wallet-table)
                                gpg-wallet-key-fingerprint gpg-wallet-url)
       (begin
         (set! gpg-wallet-table "")
         #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-wallet-initialize passphrase)
  ;; very first initialization
  (when (and (not (gpg-wallet-on?)) (not (gpg-wallet-initialized?)))
    (when (not (url-exists? (url-head gpg-wallet-url)))
      (system-mkdir (url-head gpg-wallet-url)))
    (when (not (url-exists? gpg-wallet-url))
      (system-mkdir gpg-wallet-url)
      (when (not (or (os-mingw?) (os-win32?)))
        (system-1 "chmod og-rwx" gpg-wallet-url)))
    (when (url-exists? gpg-wallet-url)
      (when (null? (gpg-secret-keys gpg-wallet-url))
	(gpg-gen-key "__TeXmacs_wallet__"
		     "TeXmacs wallet" "" passphrase
		     gpg-wallet-url))
      (with fingerprints (gpg-secret-key-fingerprints gpg-wallet-url)
	(and (nnull? fingerprints)
	     (begin
	       (set-preference "gpg wallet key fingerprint" (car fingerprints))
	       (set! gpg-wallet-table (make-ahash-table))
	       (gpg-wallet-turn-off)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reinitialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (system-remove-directory url)
  (with l (url->list (url-expand (url-complete
                                  (url-append url (url-any)) "f")))
    (for-each system-remove l))
  (rmdir (url->system url)))

(tm-define (gpg-wallet-reinitialize old-passphrase new-passphrase)
  ;; Regenerate secret key and password with no data loss.
  (and (gpg-wallet-initialized?)
       (let* ((wallet-backup
               (url-glue gpg-wallet-url
                         (string-append "-" (number->string (current-time)))))
              (b #t)
              (on? (gpg-wallet-on?)))
         (when (not on?)
           (set! b (gpg-wallet-turn-on old-passphrase)))
         (and b (with table-backup gpg-wallet-table
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
                        #f)))))))

(tm-define (gpg-wallet-destroy)
  ;; Delete the current wallet: for security all data are completely lost!
  (and (gpg-wallet-initialized?)
       ;;(with wallet-backup (url-glue gpg-wallet-url
       ;;  (string-append "-" (number->string (current-time))))
       ;;(system-move gpg-wallet-url wallet-backup))
       (begin
         (system-remove-directory gpg-wallet-url)
         (not (url-exists? gpg-wallet-url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set, get, delete entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-wallet-set key val)
  (and (gpg-wallet-on?)
       (begin
         (ahash-set! gpg-wallet-table key val)
         (gpg-wallet-save))))

(tm-define (gpg-wallet-get key)
  (and (gpg-wallet-on?)
       (ahash-ref gpg-wallet-table key)))

(tm-define (gpg-wallet-delete key)
  (and (gpg-wallet-on?)
       (begin
         (ahash-remove! gpg-wallet-table key)
         (gpg-wallet-save))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-wallet-entries)
  (and (gpg-wallet-on?)
       (ahash-table->list gpg-wallet-table)))
