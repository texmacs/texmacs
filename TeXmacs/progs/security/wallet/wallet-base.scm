
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : wallet-base.scm
;; DESCRIPTION : wallet
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security wallet wallet-base)
  (:use (security gpg gpg-wallet)))

;; So far portable implementation is based on GnuPG
(tm-define (supports-wallet?)
  (:synopsis "Tells if the platform provides a wallet implementation")
  (supports-gpg?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-initialized?)
  (:synopsis "Tells if wallet has been initialized") 
  (gpg-wallet-initialized?))

(tm-define (wallet-initialize passphrase)
  (:synopsis "Initialize a new wallet") 
  (:interactive #t)
  (:argument passphrase "password" "Wallet passphrase")
  (and (supports-wallet?) (gpg-wallet-initialize passphrase)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reinitialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-reinitialize old-passphrase new-passphrase)
  (:synopsis "Reinitialize wallet") 
  (:interactive #t)
  (:argument old-passphrase "password" "Current wallet passphrase")
  (:argument new-passphrase "password" "New wallet passphrase")
  (gpg-wallet-reinitialize old-passphrase new-passphrase))

(tm-define (wallet-destroy)
  (:synopsis "Destroy wallet") 
  (gpg-wallet-destroy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-on?)
  (:synopsis "Tells if wallet is turned on") 
  (gpg-wallet-on?))

(tm-define (wallet-off?)
  (:synopsis "Tells if wallet is turned off") 
  (not (wallet-on?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent wallet status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wallet-persistent-status "off")

(define (notify-wallet-persistent-status var val)
  (set! wallet-persistent-status val))

(define-preferences
  ("wallet persistent status" "off" notify-wallet-persistent-status))

(tm-define (wallet-persistent-status-on?)
  (== (get-preference "wallet persistent status") "on"))

(tm-define (wallet-persistent-status-off?)
  (not (wallet-persistent-status-on?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-correct-passphrase? passphrase)
  (:synopsis "Tells if @passphrase is correct") 
  (gpg-wallet-correct-passphrase? passphrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-turn-on passphrase)
  (:synopsis "Turn wallet on using @passphrase") 
  (:interactive #t)
  (:argument passphrase "password" "Wallet passphrase")
  (gpg-wallet-turn-on passphrase)
  (set-preference "wallet persistent status" "on"))

(tm-define (wallet-turn-off)
  (:synopsis "Turn wallet off") 
  (gpg-wallet-turn-off)
  (set-preference "wallet persistent status" "off"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set, get, delete entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-set key val)
  (:synopsis "Insert binding @key ~> @val into wallet")
  (gpg-wallet-set key val))

(tm-define (wallet-get key)
  (:synopsis "Get value for @key from the wallet")
  (gpg-wallet-get key))

(tm-define (wallet-delete key)
  (:synopsis "Delete binding for @key in the wallet")
  (gpg-wallet-delete key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-entries)
  (:synopsis "List all entries of the wallet")
  (gpg-wallet-entries))
