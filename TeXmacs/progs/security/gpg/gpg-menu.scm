
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gpg-menu.scm
;; DESCRIPTION : GnuPG menus
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security gpg gpg-menu)
  (:use (security gpg gpg-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encryption in tools menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: interactive encryption and decryption of all regions in the buffer

(menu-bind gpg-menu
  (when (supports-gpg?)
    (group "Public key encryption")
    ("Inline content" (tm-gpg-dialogue-insert-decrypted))
    ("Block content" (tm-gpg-dialogue-insert-decrypted-block))
    ;;("Encrypt all" (tm-gpg-dialogue-encrypt-all))
    ;;("Decrypt all" (tm-gpg-dialogue-decrypt-all))
    ;;("Collect public keys from buffer"
    ;; (tm-gpg-collect-public-keys-from-buffer))
    ---
    (group "Passphrase encryption")
    ("Inline content" (tm-gpg-insert-passphrase-decrypted))
    ("Block content" (tm-gpg-insert-passphrase-decrypted-block))
    ;;("Encrypt all" (tm-gpg-dialogue-passphrase-encrypt-all))
    ;;("Decrypt all" (tm-gpg-dialogue-passphrase-decrypt-all))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-insert? t)
  (:require (or (tm-gpg-decrypted? t)
                (tm-gpg-encrypted? t)
                (tm-gpg-passphrase-decrypted? t)
                (tm-gpg-passphrase-encrypted? t)))
  #f)

(tm-define (focus-can-remove? t)
  (:require (or (tm-gpg-decrypted? t)
                (tm-gpg-encrypted? t)
                (tm-gpg-passphrase-decrypted? t)
                (tm-gpg-passphrase-encrypted? t)))
  #f)

(tm-define (focus-can-insert-remove? t)
  (:require (or (tm-gpg-decrypted? t)
                (tm-gpg-encrypted? t)
                (tm-gpg-passphrase-decrypted? t)
                (tm-gpg-passphrase-encrypted? t)))
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-variants-of t)
  (:require (tree-in? t '(gpg-decrypted-block gpg-decrypted)))
  (list 'gpg-decrypted 'gpg-decrypted-block))

(tm-define (focus-variants-of t)
  (:require (tree-in? t '(gpg-passphrase-decrypted-block
			  gpg-passphrase-decrypted)))
  (list 'gpg-passphrase-decrypted 'gpg-passphrase-decrypted-block))

(tm-define (focus-variants-of t)
  (:require (tree-in? t '(gpg-encrypted-block gpg-encrypted)))
  (list 'gpg-encrypted 'gpg-encrypted-block))

(tm-define (focus-variants-of t)
  (:require (tree-in? t '(gpg-passphrase-encrypted-block
			  gpg-passphrase-encrypted)))
  (list 'gpg-passphrase-encrypted 'gpg-passphrase-encrypted-block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-table style-synopsis
  ("gpg-info-level-none"     "Hide details")
  ("gpg-info-level-short"    "Hide recipients")
  ("gpg-info-level-detailed" "Show details"))

(tm-define (standard-options l)
  (:require (or (tm-gpg-symbol-decrypted? l)
                (tm-gpg-symbol-passphrase-decrypted? l)))
  (list "gpg-info-level-none"
	"gpg-info-level-short"
        "gpg-info-level-detailed"))

(tm-define (parameter-show-in-menu? l)
  (:require (== l "gpg-info-level"))
  #f)

(tm-define (style-category p)
  (:require (in? p (list "gpg-info-level-none"
			 "gpg-info-level-short"
                         "gpg-info-level-detailed")))
  :gpg-decrypted-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-menu t)
  (:require (tm-gpg-decrypted? t))
  ("Encrypt" (tm-gpg-encrypt (focus-tree))))

(tm-menu (focus-toggle-menu t)
  (:require (tm-gpg-passphrase-decrypted? t))
  ("Encrypt" (tm-gpg-dialogue-passphrase-encrypt (focus-tree))))

(tm-menu (focus-toggle-menu t)
  (:require (tm-gpg-encrypted? t))
  ("Decrypt" (tm-gpg-dialogue-decrypt (focus-tree))))

(tm-menu (focus-toggle-menu t)
  (:require (tm-gpg-passphrase-encrypted? t))
  ("Decrypt" (tm-gpg-dialogue-passphrase-decrypt (focus-tree))))


(tm-menu (focus-toggle-icons t)
  (:require (tm-gpg-decrypted? t))
  ((check (balloon (icon "tm_lock_open.xpm") "Decrypted") "v"
          (alternate-second? (focus-tree)))
   (tm-gpg-encrypt (focus-tree))))

(tm-menu (focus-toggle-icons t)
  (:require (tm-gpg-passphrase-decrypted? t))
  ((check (balloon (icon "tm_lock_open.xpm") "Decrypted") "v"
          (alternate-second? (focus-tree)))
   (tm-gpg-dialogue-passphrase-encrypt (focus-tree))))

(tm-menu (focus-toggle-icons t)
  (:require (tm-gpg-encrypted? t))
  ((check (balloon (icon "tm_lock_closed.xpm") "Encrypted") "v"
          (alternate-second? (focus-tree)))
   (tm-gpg-dialogue-decrypt (focus-tree))))

(tm-menu (focus-toggle-icons t)
  (:require (tm-gpg-passphrase-encrypted? t))
  ((check (balloon (icon "tm_lock_closed.xpm") "Encrypted") "v"
          (alternate-second? (focus-tree)))
   (tm-gpg-dialogue-passphrase-decrypt (focus-tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alternate toggle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (tm-gpg-decrypted? t))
  (tm-gpg-encrypt t))

(tm-define (alternate-toggle t)
  (:require (tm-gpg-encrypted? t))
  (tm-gpg-dialogue-decrypt t))

(tm-define (alternate-toggle t)
  (:require (tm-gpg-passphrase-decrypted? t))
  (tm-gpg-dialogue-passphrase-encrypt t))

(tm-define (alternate-toggle t)
  (:require (tm-gpg-passphrase-encrypted? t))
  (tm-gpg-dialogue-passphrase-decrypt t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hidden icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-hidden-icons t)
  (:require (tm-gpg-decrypted? t)))

(tm-menu (focus-hidden-icons t)
  (:require (tm-gpg-encrypted? t)))

(tm-menu (focus-hidden-icons t)
  (:require (tm-gpg-passphrase-decrypted? t)))

(tm-menu (focus-hidden-icons t)
  (:require (tm-gpg-passphrase-encrypted? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-extra-menu t)
  (:require (tm-gpg-decrypted? t))
  ---
  ("Recipients" (tm-gpg-dialogue-replace-decrypted (focus-tree))))

(tm-menu (focus-extra-icons t)
  (:require (tm-gpg-decrypted? t))
  //
  ("Recipients" (tm-gpg-dialogue-replace-decrypted (focus-tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encryption of whole buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-icons t)
  (:require (tree-in? t '(gpg-passphrase-encrypted-buffer)))
  ((check (balloon (icon "tm_lock_closed.xpm") "Encrypted buffer") "v"
          (alternate-second? (focus-tree)))
   (tm-gpg-dialogue-passphrase-decrypt-buffer (current-buffer))))

(menu-bind document-encryption-menu
  (when (supports-gpg?)
    (if (== (get-init "encryption") "")
	("Passphrase encryption"
	 (tm-gpg-dialogue-passphrase-buffer-set-encryption)))
    (if (== (get-init "encryption") "gpg-passphrase")
	("New passphrase"
	 (tm-gpg-dialogue-passphrase-buffer-set-encryption))
	("Disable encryption"
	 (tm-gpg-passphrase-buffer-unset-encryption)))))


