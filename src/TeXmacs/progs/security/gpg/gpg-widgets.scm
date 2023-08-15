
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gpg-widgets.scm
;; DESCRIPTION : Widgets related to GnuPG
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security gpg gpg-widgets)
  (:use (security gpg gpg-base)
	(security wallet wallet-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ahash-entries h)
  (map first (ahash-table->list h)))

(define (ahash-values h)
  (map cdr (ahash-table->list h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default identity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpg-default-key-fingerprint "")

(define (notify-gpg-default-key-fingerprint var val)
  (set-user-info "gpg-key-fingerprint" val)
  (set! gpg-default-key-fingerprint val))

(define-preferences
  ("gpg default key fingerprint" "" notify-gpg-default-key-fingerprint))

(tm-define (gpg-get-default-key-fingerprint)
  (:secure #t)
  (:synopsis "Get default key fingerprint")
  (if (or (gpg-secret-key-fingerprint? gpg-default-key-fingerprint)
	  (== gpg-default-key-fingerprint ""))
      gpg-default-key-fingerprint
      (begin
	 (set-user-info "gpg-key-fingerprint" "")
	 "")))

(tm-define (gpg-set-default-key-fingerprint fingerprint)
  (:secure #t)
  (:interactive #t)
  (:synopsis "Set default key fingerprint")
  (:argument fingerprint "Key fingerprint")
  (when (or (gpg-secret-key-fingerprint? fingerprint)
	    (== fingerprint ""))
    (refresh-now "identity-info")
    (set-preference "gpg default key fingerprint" fingerprint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create new public/secret key pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Warn if weak passphrase
(define (char-special? c)
  (not (or (char-alphabetic? c) (char-numeric? c))))

(define (gpg-weak-passphrase? passphrase)
  (or (< (string-length passphrase) 8)
      (= (string-count passphrase char-upper-case?) 0)
      (= (string-count passphrase char-lower-case?) 0)
      (= (string-count passphrase char-numeric?) 0)
      (= (string-count passphrase char-special?) 0)))

(tm-widget ((gpg-widget-gen-key callback) cmd)
  (let* ((err "")
	 (name "")
	 (email "")
	 (comment "")
	 (passphrase ""))
  (resize '("550px" "550px" "9999px") '("250px" "250px" "9999px")
    (padded
      (refreshable "gpg-refreshable-gen-key"
      (if (== err "weak passphrase") (centered
        (text "Passphrases are expected to have at least: 8 characters,")
        (text "one numeric character, one upper case letter,")
        (text "one lower case letter, one non alpha-numeric symbol.")
        (bold (text "Click Ok button twice to discard this warning."))))
      (centered (if (== err "short name")
        (bold (text "GnuPG requires name and email to have at least 5 characters")))))
      (form "Generate new GnuPG key"
        (aligned
          (item (text "Name:")
            (form-input "name" "string"
			(list (get-user-info "name")) "10w"))
          (item (text "Email:")
            (form-input "email" "string"
			(list (get-user-info "email")) "10w"))
          (item (text "Comment:")
            (form-input "comment" "string" (list "") "10w"))
          (item (text "Passphrase:")
            (form-input "passphrase" "password" (list "") "10w")))
        (bottom-buttons
          ("Cancel" (cmd (list "cancel"))) 
          >>
          ("Ok"
           (with n (length (form-values))
            (if (and (> n 0) (string? (first (form-values))))
                (set! name (first (form-values))))
            (if (and (> n 1) (string? (second (form-values))))
                (set! email (second (form-values))))
            (if (and (> n 2) (string? (third (form-values))))
                (set! comment (third (form-values))))
            (if (and (> n 3) (string? (fourth (form-values))))
                (set! passphrase (fourth (form-values))))
            (if (or (< (string-length name) 5)
                    (< (string-length email) 5))
              (begin (set! err "short name")
                     (refresh-now "gpg-refreshable-gen-key"))
              (if (and (!= err "weak passphrase")
                       (gpg-weak-passphrase? passphrase))
                (begin (set! err "weak passphrase")
                       (refresh-now "gpg-refreshable-gen-key"))
                (begin (system-wait "Generating new identity" "please wait")
		       (gpg-gen-key name email comment passphrase)
		       (with fprs (gpg-secret-key-fingerprints)
                         (when (and (list? fprs) (nnull? fprs))
                           (when (wallet-persistent-status-on?)
			     (with-wallet
			       (wallet-set (list "gpg" (last fprs))
					   passphrase)))
			   (when (and (nnull? fprs) (null? (cdr fprs)))
			     (gpg-set-default-key-fingerprint (car fprs)))))
		       (callback)
		       (cmd "Ok"))))))))))))

(tm-define (gpg-dialogue-gen-key . callback)
  (:secure #t)
  (:synopsis "Generate GnuPG key")
  (with cb (if (null? callback) noop (car callback))
    (dialogue-window (gpg-widget-gen-key cb) noop "Generate GnuPG key")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select public keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define gpg-widget-selected-public-key-fingerprints (make-ahash-table))

(tm-widget (gpg-widget-select-public-key-fingerprints cmd)
  (let* ((key->string
	  (lambda (k) (string-append (gpg-get-key-user-id k)
				     ", " (gpg-get-key-fingerprint k))))
	 (keys (gpg-public-keys))
	 (sels gpg-widget-selected-public-key-fingerprints))
    (resize '("700px" "700px" "9999px") '("200px" "200px" "9999px") 
      (padded
	(scrollable
	  (padded
	    (for (x keys)
	      (aligned
		(item (toggle
		       (if answer
			   (ahash-set! sels (gpg-get-key-fingerprint x) #t)
			   (ahash-remove! sels (gpg-get-key-fingerprint x)))
		       (ahash-ref sels (gpg-get-key-fingerprint x)))
		  (hlist // // (text (key->string x))))))
	    (glue #f #t 0 0)))
	===
	(bottom-buttons
	  ("Cancel" (cmd (list "Cancel")))
	  >>
	  ("Ok"
	   (set! gpg-widget-selected-public-key-fingerprints sels)
	   (cmd (cons "Ok" (ahash-entries sels)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-command-encrypt data callback action)
  (when (and (list? action) (nnull? action) (== (car action) "Ok"))
    (callback (gpg-encrypt data (cdr action)))))

(tm-define (gpg-dialogue-encrypt data callback)
  (:secure #t)
  (:synopsis "Interactive GnuPG encryption")
  (dialogue-window gpg-widget-select-public-key-fingerprints
		   (lambda (x) (gpg-command-encrypt data callback x))
		   "Select GnuPG recipients"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ask and remember password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((gpg-widget-ask-passphrase fingerprint) cmd)
  (let* ((error #f)
         (key (gpg-search-secret-key-by-fingerprint fingerprint))
         (uid (gpg-get-key-user-id key)))
  (resize "500px" "200px"
  (refreshable "gpg-refreshable-ask-passphrase"
  (padded
    (if error (hlist >> (bold (text "Wrong passphrase")) >>))
    (centered
      (text "Enter passphrase for identity")
      (bold (text uid))
      (text "of fingerprint")
      (text fingerprint))
    === ===
  (form "Ask passphrase"
    (form-input "passphrase" "password" '() "10w")
    (bottom-buttons
      ("Cancel"
       (cmd (list "Cancel")))
      >>
      ("Ok"
       (with n (length (form-values))
         (if (and (> n 0) (string? (first (form-values)))
                  (gpg-correct-passphrase? fingerprint
                                           (first (form-values))))
             (begin
               (with-wallet
                 (wallet-set (list "gpg" fingerprint) (first (form-values))))
               (cmd (cons "Ok" (form-values))))
             (begin
               (set! error #t)
               (refresh-now "gpg-refreshable-ask-passphrase"))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((gpg-widget-wrong-fingerprint fingerprint) cmd)
  (resize "500px" "120px"
    (padded
      (centered (bold (text "Unknown secret key fingerprint")))
      (centered (bold (text fingerprint)))
      ===
     (bottom-buttons
      >>
      ("Ok" (cmd "Ok"))))))

(define (gpg-command-decrypt data callback args)
  (when (and (list? args) (= (length args) 2) (== (car args) "Ok"))
    (let* ((passphrase (second args))
	   (dec (gpg-decrypt data passphrase)))
      (when dec (callback dec)))))

(tm-define (gpg-dialogue-decrypt data fingerprint callback)
  (:secure #t)
  (:synopsis "Interactive GnuPG decryption")
  (with-wallet
    (with passphrase (wallet-get (list "gpg" fingerprint))
      (if passphrase
	  (gpg-command-decrypt data callback (list "Ok" passphrase))
	  (with key (gpg-search-secret-key-by-fingerprint fingerprint)
	    (if key
		(dialogue-window (gpg-widget-ask-passphrase fingerprint)
		  (lambda (x) (gpg-command-decrypt data callback x))
		  "Decryption")
		(dialogue-window (gpg-widget-wrong-fingerprint fingerprint)
				 noop "Decryption")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Passphrase encryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (gpg-widget-ask-new-passphrase cmd)
  (with error ""
    (resize "500px" "180px"
      (refreshable "gpg-refreshable-ask-new-passphrase"
	(padded
	  (if (== error "mismatch")
	      (hlist >> (bold (text "Passphrases mismatch")) >>))
	  (if (== error "empty")
	      (hlist >> (bold (text "Wrong empty passphrase")) >>))
	  (centered (text "Enter new passphrase for encryption"))
	  === ===
	  (form "Ask new passphrase"
	    (aligned
	      (item (text "New passphrase:")
		(form-input "passphrase1" "password" '("") "10w"))
	      (item (text "Retype passphrase:")
		(form-input "passphrase2" "password" '("") "10w")))
	    (bottom-buttons
	      ("Cancel"
	       (cmd (list "Cancel")))
	      >>
	      ("Ok"
	       (let* ((n (length (form-values)))
		      (passwd1 (if (and (> n 0)
					(string? (first (form-values))))
				   (first (form-values)) ""))
		      (passwd2 (if (and (> n 1)
					(string? (second (form-values))))
				   (second (form-values)) "")))
		 (if (== passwd1 passwd2)
		     (if (!= passwd1 "")
			 (cmd (list "Ok" passwd1))
			 (begin
			   (set! error "empty")
			   (refresh-now
			    "gpg-refreshable-ask-new-passphrase")))
		     (begin
		       (set! error "mismatch")
		       (refresh-now
			"gpg-refreshable-ask-new-passphrase"))))))))))))

(define (gpg-command-passphrase-encrypt data callback action)
  (when (and (list? action) (nnull? action) (== (first action) "Ok"))
    (callback (gpg-passphrase-encrypt data (second action)))))

(tm-define (gpg-dialogue-passphrase-encrypt data callback)
  (:secure #t)
  (:synopsis "Interactive GnuPG passphrase encryption")
  (dialogue-window gpg-widget-ask-new-passphrase
    (lambda (x) (gpg-command-passphrase-encrypt data callback x))
    "Passphrase encryption"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Passphrase decryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((gpg-widget-ask-standalone-passphrase check) cmd)
  (with error #f
  (resize "500px" "150px"
    (refreshable "gpg-refreshable-ask-standalone-passphrase"
      (padded
        (if error (hlist >> (bold (text "Wrong passphrase")) >>))
        (centered (text "Enter passphrase for decryption"))
    === ===
  (form "Ask standalone passphrase"
    (form-input "passphrase" "password" '() "10w")
    (bottom-buttons
      ("Cancel"
       (cmd (list "Cancel")))
      >>
      ("Ok"
       (with n (length (form-values))
         (if (and (> n 0) (string? (first (form-values))))
           (with passphrase (first (form-values))
             (if (check passphrase) 
               (cmd (list "Ok" passphrase))
               (begin
                 (set! error #t)
                 (refresh-now
		  "gpg-refreshable-ask-standalone-passphrase"))))))))))))))

(define (gpg-command-passphrase-decrypt data callback action)
  (when (and (list? action) (nnull? action) (== (first action) "Ok"))
    (callback (gpg-passphrase-decrypt data (second action)))))

(tm-define (gpg-dialogue-passphrase-decrypt data callback)
  (:secure #t)
  (:synopsis "Interactive GnuPG passphrase decryption")
  (dialogue-window
   (gpg-widget-ask-standalone-passphrase
     (lambda (x) (gpg-decryptable? data x)))
    (lambda (x) (gpg-command-passphrase-decrypt data callback x))
    "Passphrase encryption"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secret key manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((gpg-widget-confirm-delete-secret-keys cb) cmd)
  (padded
    (bold (text "Are you sure you want to delete selected secret keys?"))
    (bold (text "These secret keys will be definitely lost!")))
  (bottom-buttons
    ("Cancel" (cmd "Cancel"))
    >>
    ("Delete secret keys" (cb) (cmd "Ok"))))

(tm-widget (gpg-widget-manage-secret-keys)
  (let* ((key->string
	  (lambda (k) (string-append " " (gpg-get-key-user-id k)
				     ", " (gpg-get-key-fingerprint k))))
	 (keys (gpg-secret-keys))
	 (sels (make-ahash-table)))
    (refreshable "gpg-refreshable-manage-secret-keys"
      (scrollable
	(padded
	  (for (x (with k (gpg-get-default-key-fingerprint)
		    (if (!= k "")
			(list (gpg-search-key-by-fingerprint k keys))
			(list))))
	    (aligned
	      (item (toggle
		     (begin
		       (if answer
			   (ahash-set! sels x #t)
			   (ahash-remove! sels x))
		       (refresh-now "gpg-refreshable-manage-secret-keys"))
		     (ahash-ref sels x))
		(hlist // // (bold (text (key->string x)))))))
	  (for (x (with y (gpg-get-default-key-fingerprint)
		    (filter (lambda (k) (!= (gpg-get-key-fingerprint k) y))
			    keys)))
	    (aligned
	      (item (toggle
		     (begin
		       (if answer
			   (ahash-set! sels x #t)
			   (ahash-remove! sels x))
		       (refresh-now "gpg-refreshable-manage-secret-keys"))
		     (ahash-ref sels x))
		(hlist // // (text (key->string x))))))
	  (glue #f #t 0 5)))
      ===
      (hlist
	(explicit-buttons
	  (when (= (ahash-size sels) 0)
	    ("New identity"
	     (with cb (lambda ()
			(set! keys (gpg-secret-keys))
			(refresh-now "gpg-refreshable-manage-secret-keys")
			(set! gpg-widget-manage-public-keys-variable-keys
			      (gpg-public-keys))
			(refresh-now "gpg-refreshable-manage-public-keys"))
	       (gpg-dialogue-gen-key cb))))
	  // //
	  (when (= (ahash-size sels) 1)
	    ("Set default"
	     (gpg-set-default-key-fingerprint
	      (gpg-get-key-fingerprint (car (ahash-entries sels))))
	     (set! sels (make-ahash-table))
	     (refresh-now "gpg-refreshable-manage-secret-keys")))
	  // //
	  (when (> (ahash-size sels) 0)
	    ("Delete"
	     (with cb (lambda ()
			(map gpg-delete-secret-and-public-key
			     (map gpg-get-key-fingerprint
				  (ahash-entries sels)))
			(when (member (gpg-get-default-key-fingerprint)
				      (ahash-entries sels))
			  (gpg-set-default-key-fingerprint ""))
			(set! keys (gpg-secret-keys))
			(set! sels (make-ahash-table))
			(refresh-now "gpg-refreshable-manage-secret-keys")
			(set! gpg-widget-manage-public-keys-variable-keys
			      (gpg-public-keys))
			(refresh-now "gpg-refreshable-manage-public-keys"))
	       (dialogue-window (gpg-widget-confirm-delete-secret-keys cb)
				noop "Confirm secret key deletion"))))
	  >>
	  (when (> (ahash-size sels) 0)
	    ("Export"
	     (with fingerprints
		 (map gpg-get-key-fingerprint (ahash-entries sels))
	       (choose-file
		(lambda (x)
		  (string-save
		   (gpg-export-secret-keys fingerprints) x)
		  (set! sels (make-ahash-table))
		  (refresh-now "gpg-refreshable-manage-secret-keys"))
		"Export selected secret keys to file" "gpg"))))
	  // //
	  (when (= (ahash-size sels) 0)
	    ("Import"
	     (begin (choose-file
		     (lambda (x)
		       (gpg-import-secret-keys (string-load x))
		       (set! keys (gpg-secret-keys))
		       (refresh-now "gpg-refreshable-manage-secret-keys")
		       (set! gpg-widget-manage-public-keys-variable-keys
			     (gpg-public-keys))
		       (refresh-now "gpg-refreshable-manage-public-keys"))
		     "Import secret keys" "gpg"))))
	  ;;// //
	  ;;(when (nnull? keys)
	  ;;  ("Select all"
	  ;;    (for (x keys) (ahash-set! sels x #t))
	  ;;    (refresh-now "gpg-refreshable-manage-secret-keys")))
	  ;;// //
	  ;;(when (> (ahash-size sels) 0)
	  ;;  ("Reset all"
	  ;;    (set! sels (make-ahash-table))
	  ;;    (refresh-now "gpg-refreshable-manage-secret-keys")))
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public key manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpg-widget-manage-public-keys-variable-keys (list))

(tm-widget ((gpg-widget-confirm-delete-public-keys cb) cmd)
  (padded
    (bold (text "Are you sure you want to delete selected public keys?"))
    (bold (text "These public keys will be definitely lost!")))
  (bottom-buttons
    ("Cancel" (cmd "Cancel"))
    >>
    ("Delete public keys" (cb) (cmd "Ok"))))

(tm-widget (gpg-widget-manage-public-keys)
  (let* ((key->string
	  (lambda (k) (string-append " " (gpg-get-key-user-id k)
				     ", " (gpg-get-key-fingerprint k))))
	 (sels (begin
		 (set! gpg-widget-manage-public-keys-variable-keys
		       (gpg-public-keys))
		 (make-ahash-table))))
    (refreshable "gpg-refreshable-manage-public-keys"
      (scrollable
	(padded
	  (for (x gpg-widget-manage-public-keys-variable-keys)
	    (aligned
	      (item (toggle
		     (begin
		       (if answer
			   (ahash-set! sels x #t)
			   (ahash-remove! sels x))
		       (refresh-now "gpg-refreshable-manage-public-keys"))
		     (ahash-ref sels x))
		(hlist // // (text (key->string x))))))
	  (glue #f #t 0 5)))
      ===
      (hlist
	(explicit-buttons
	  (when (> (ahash-size sels) 0)
	    ("Delete"
	     (with cb (lambda ()
			(map gpg-delete-public-key
			     (map gpg-get-key-fingerprint
				  (ahash-entries sels)))
			(set! gpg-widget-manage-public-keys-variable-keys
			      (gpg-public-keys))
			(set! sels (make-ahash-table))
			(refresh-now "gpg-refreshable-manage-public-keys"))
	       (dialogue-window (gpg-widget-confirm-delete-public-keys cb)
				noop "Confirm public key deletion"))))
	  >>
	  (when (> (ahash-size sels) 0)
	    ("Export"
	     (with fingerprints
		 (map gpg-get-key-fingerprint (ahash-entries sels))
	       (choose-file
		(lambda (x)
		  (string-save
		   (gpg-export-public-keys fingerprints) x)
		  (set! sels (make-ahash-table))
		  (refresh-now "gpg-refreshable-manage-public-keys"))
		"Export selected public keys to file" "gpg"))))
	  // //
	  (when (= (ahash-size sels) 0)
	    ("Import"
	     (begin (choose-file
		     (lambda (x)
		       (gpg-import-public-keys (string-load x))
		       (set! gpg-widget-manage-public-keys-variable-keys
			     (gpg-public-keys))
		       (refresh-now "gpg-refreshable-manage-public-keys"))
		     "Import public keys" "gpg"))))
	  ;;// //
	  ;;(when (nnull? gpg-widget-manage-public-keys-variable-keys)
	  ;;  ("Select all"
	  ;;    (for (x gpg-widget-manage-public-keys-variable-keys)
	  ;;      (ahash-set! sels x #t))
	  ;;    (refresh-now "gpg-refreshable-manage-public-keys")))
	  ;;// //
	  ;;(when (> (ahash-size sels) 0)
	  ;;  ("Reset all"
	  ;;    (set! sels (make-ahash-table))
	  ;;    (refresh-now "gpg-refreshable-manage-public-keys")))
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collected key manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((gpg-widget-confirm-delete-collected-public-keys cb) cmd)
  (padded
    (bold (text "Are you sure you want to delete collected public keys?"))
    (bold (text "These collected public keys will be definitely lost!")))
  (bottom-buttons
    ("Cancel" (cmd "Cancel"))
    >>
    ("Delete collected public keys" (cb) (cmd "Ok"))))

(tm-widget (gpg-widget-manage-collected-public-keys)
  (let* ((fingerprint->string
	  (lambda (x) (string-append
		       " " (gpg-collected-public-key-uid x) ", " x)))
	 (fingerprints (begin
			 (tm-gpg-collect-public-keys-from-buffer)
			 (gpg-collected-public-key-fingerprints)))
	 (sels (make-ahash-table)))
    (refreshable "gpg-refreshable-manage-collected-public-keys"
      (scrollable
	(padded
	  (for (x fingerprints)
	    (aligned
	      (item (toggle
		     (begin
		       (if answer
			   (ahash-set! sels x #t)
			   (ahash-remove! sels x))
		       (refresh-now
			"gpg-refreshable-manage-collected-public-keys"))
		     (ahash-ref sels x))
		(hlist // // (text (fingerprint->string x))))))
	  (glue #f #t 0 5)))
      ===
      (hlist
	(explicit-buttons
	  (when (> (ahash-size sels) 0)
	    ("Delete"
	     (with cb (lambda ()
			(gpg-delete-collected-public-keys (ahash-entries sels))
			(set! fingerprints
			      (gpg-collected-public-key-fingerprints))
			(set! sels (make-ahash-table))
			(refresh-now
			 "gpg-refreshable-manage-collected-public-keys"))
	       (dialogue-window
		(gpg-widget-confirm-delete-collected-public-keys cb)
		noop "Confirm collected public key deletion"))))
	  >>
	  (when (> (ahash-size sels) 0)
	    ("Import"
	     (map gpg-import-public-key-from-collected (ahash-entries sels))
	     (map gpg-delete-collected-public-key (ahash-entries sels))
	     (set! fingerprints (gpg-collected-public-key-fingerprints))
	     (set! sels (make-ahash-table))
	     (refresh-now "gpg-refreshable-manage-collected-public-keys")
	     (set! gpg-widget-manage-public-keys-variable-keys
		   (gpg-public-keys))
	     (refresh-now "gpg-refreshable-manage-public-keys")))
	  ;;// //
	  ;;(when (nnull? fingerprints)
	  ;;  ("Select all"
	  ;;    (for (x fingerprints) (ahash-set! sels x #t))
	  ;;    (refresh-now "gpg-refreshable-manage-collected-public-keys")))
	  ;;// //
	  ;;(when (> (ahash-size sels) 0)
	  ;;  ("Reset all"
	  ;;    (set! sels (make-ahash-table))
	  ;;    (refresh-now "gpg-refreshable-manage-collected-public-keys")))
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (gpg-widget-manage-keys)
  (padded
    (resize '("700px" "700px" "9999px") '("300px" "400px" "9999px")
      (tabs
	(tab (text "Secret keys")
	  (centered
	    (dynamic (gpg-widget-manage-secret-keys))))
	(tab (text "Public keys")
	  (centered
	    (dynamic (gpg-widget-manage-public-keys))))
	(tab (text "Collected keys")
	  (centered
	    (dynamic (gpg-widget-manage-collected-public-keys))))))))

(tm-define (open-gpg-key-manager)
  (top-window gpg-widget-manage-keys "Key manager"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (gpg-not-supported-preferences-widget)
  (centered
    (text "Encryption facilities are not currently available.")
    (text "Please install GnuPG software from https://www.gnupg.org/")
    (text "or give the complete path to the \"gpg\" or \"gpg2\" command."))
  ===
  (hlist
    (text "GnuPG command:") // //
    (enum (gpg-set-executable answer)
          (list "" "gpg" "gpg2" "")
          "" "15em")))

(tm-widget (gpg-supported-preferences-widget)
  (hlist
    (text "GnuPG command:") // //
    (enum (gpg-set-executable answer)
          (list (gpg-get-executable) "gpg" "gpg2" "")
          (gpg-get-executable) "15em")
    >>)
  ===
  (hlist
    (text "Cipher algorithm for passphrase encryption:") // //
    (enum (gpg-set-cipher-algorithm answer)
          (list "AES192" "AES256")
          (gpg-get-cipher-algorithm) "7em")
    >>))

(tm-widget (gpg-preferences-widget)
  (if (supports-gpg?)
      (dynamic (gpg-supported-preferences-widget)))
  (if (not (supports-gpg?))
      (dynamic (gpg-not-supported-preferences-widget))))
