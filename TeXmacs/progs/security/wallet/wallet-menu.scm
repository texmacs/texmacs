
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : wallet-menu.scm
;; DESCRIPTION : wallet
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security wallet wallet-menu)
  (:use (security wallet wallet-base)))

(when (os-macos?) 
  (use-modules (security keychain macos-security)))

(when (or (os-mingw?) (os-win32?))
  (use-modules (security keychain win-security)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember wallet master passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wallet-service "TeXmacs")
(define wallet-account "wallet")

(tm-define (wallet-can-remember-passphrase?)
  (or
   (and (os-macos?) (url-exists-in-path? "security"))
   (and (os-mingw?) (url-exists-in-path? "winwallet"))
   (and (os-win32?) (url-exists-in-path? "winwallet"))))

(tm-define (wallet-save-passphrase passphrase)
  (and (wallet-can-remember-passphrase?)
       (begin
         (when (system-security-quiet-find-generic-password
                wallet-account wallet-service)
           (system-security-delete-generic-password
            wallet-account wallet-service))
         (system-security-add-generic-password
          wallet-account wallet-service passphrase))))

(tm-define (wallet-load-passphrase)
  (and (wallet-can-remember-passphrase?)
       (system-security-quiet-find-generic-password
        wallet-account wallet-service)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize wallet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-special? c)
  (not (or (char-alphabetic? c) (char-numeric? c))))

(define (wallet-weak-passphrase? passphrase)
  (or (< (string-length passphrase) 8)
      (= (string-count passphrase char-upper-case?) 0)
      (= (string-count passphrase char-lower-case?) 0)
      (= (string-count passphrase char-numeric?) 0)
      (= (string-count passphrase char-special?) 0)))

(define (wallet-strong-passphrase? passphrase)
  (not (wallet-weak-passphrase? passphrase)))

(tm-widget (wallet-widget-initialize cmd)
  (with wallet-widget-weak-passphrase? #f
    (resize "500px" "200px"
      (padded
        (form "Ask passphrase"
          (hlist
            (text "Wallet passphrase:") // //
            (form-input "passphrase" "password" '() "10w") >>)
          (when (wallet-can-remember-passphrase?)
            ===
            (hlist
              (text "Retrieve wallet passphrase automatically at login?") // //
              (form-enum "remember?" '("yes" "no") "no" "4em") >>))
          (refreshable "wallet-widget-reask-passphrase"
            (if wallet-widget-weak-passphrase?
                (centered
                  (text "Warning: weak passphrase, click Ok again to confirm.")
                  (text "Passphrase should have at least 8 characters:")
                  (text "1 upper case, 1 lower case, 1 digit, and 1 symbol."))))
          (bottom-buttons
            >>
            ("Cancel" (cmd "Cancel")) // //
            ("Ok"
             (with n (length (form-values))
               (when (and (> n 0) (string? (first (form-values)))
                          (or wallet-widget-weak-passphrase?
                              (wallet-strong-passphrase?
                               (first (form-values)))))
                 (system-wait "Initializing wallet" "please wait")
                 (when (and (wallet-initialize (first (form-values)))
                            (== (second (form-values)) "yes"))
                   (wallet-save-passphrase (first (form-values))))
                 (cmd "Ok")))
             (set! wallet-widget-weak-passphrase? #t)
             (refresh-now "wallet-widget-reask-passphrase"))))))))

(tm-define (wallet-dialogue-initialize . callback)
  (with cb (if (null? callback) noop (car callback))
    (dialogue-window wallet-widget-initialize cb "Initialize wallet")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reinitialize wallet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (wallet-widget-reinitialize cmd)
  (with wallet-widget-weak-passphrase? #f
    (resize "500px" "200px"
      (padded
        (form "Ask new passphrase"
          (hlist
            (text "New wallet passphrase:") // //
            (form-input "passphrase" "password" '() "10w") >>)
          (when (wallet-can-remember-passphrase?)
            ===
            (hlist
              (text "Retrieve wallet passphrase automatically at login?") // //
              (form-enum "remember?" '("yes" "no") "no" "4em") >>))
          (refreshable "wallet-widget-reask-new-passphrase"
            (if wallet-widget-weak-passphrase?
                (centered
                  (text "Warning: weak passphrase, click Ok again to confirm.")
                  (text "Passphrase should have at least 8 characters:")
                  (text "1 upper case, 1 lower case, 1 digit, and 1 symbol."))))
          (bottom-buttons
            >>
            ("Cancel" (cmd "Cancel")) // //
            ("Ok"
             (with n (length (form-values))
               (when (and (> n 0) (string? (first (form-values)))
                          (or wallet-widget-weak-passphrase?
                              (wallet-strong-passphrase?
                               (first (form-values)))))
                 (system-wait "Reinitializing wallet" "please wait")
                 (when (wallet-reinitialize "" (first (form-values)))
                   (when (== (second (form-values)) "yes")
                     (wallet-save-passphrase (first (form-values)))))
                 (cmd "Ok")))
             (set! wallet-widget-weak-passphrase? #t)
             (refresh-now "wallet-widget-reask-new-passphrase"))))))))

(tm-define (wallet-dialogue-reinitialize . callback)
  (let* ((cb1 (if (null? callback) noop (car callback)))
	 (cb2 (lambda (x)
		(when (== x "Ok") (dialogue-window
				   wallet-widget-reinitialize cb1
				   "Change wallet passphrase")))))
    (wallet-dialogue-turn-on cb2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ask passphrase and turn on wallet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (wallet-widget-turn-on cmd)
  (with wallet-widget-wrong-passphrase? #f
    (resize "500px" "150px"
      (padded
        (refreshable "wallet-widget-reask-passphrase"
          (if wallet-widget-wrong-passphrase?
              (centered (bold (text "Wrong passphrase")))))
        (form "Ask passphrase"
          (hlist
            (text "Wallet passphrase:") // //
            (form-input "passphrase" "password" '() "2w") >>)
          ===
          (when (wallet-can-remember-passphrase?)
            (hlist
              (text "Retrieve wallet passphrase automatically at login?") // //
              (form-enum "remember?" '("yes" "no") "no" "4em") >>))
          === ===
          (bottom-buttons
            >>
            ("Cancel" (wallet-turn-off) (cmd "Cancel")) // //
            ("Ok"
             (with n (length (form-values))
               (when (and (> n 0) (string? (first (form-values))))
                 (when (and (wallet-correct-passphrase? (first (form-values)))
                            (when (and (wallet-turn-on (first (form-values)))
                                       (== (second (form-values)) "yes"))
                              (wallet-save-passphrase (first (form-values))))
                            (cmd "Ok")))))
             (set! wallet-widget-wrong-passphrase? #t)
             (refresh-now "wallet-widget-reask-passphrase"))))))))

(tm-define (wallet-dialogue-turn-on . callback)
  (let* ((cb (if (null? callback) noop (car callback)))
	 (passphrase (wallet-load-passphrase)))
    (if (and passphrase (!= passphrase "")
	     (wallet-correct-passphrase? passphrase))
	(begin (wallet-turn-on passphrase) (cb "Ok"))
	(dialogue-window wallet-widget-turn-on cb "Turn on wallet"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destroy wallet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (wallet-widget-destroy cmd)
  (padded
    (bold (text "Are you sure you want to destroy the wallet?"))
    (bold (text "All saved passphrases will be lost forever!"))
    ===
    (bottom-buttons
      >>
      ("Cancel" (cmd "Cancel")) // //
      ("Destroy wallet" (wallet-destroy) (cmd "Ok")))))

(tm-define (wallet-dialogue-destroy . callback)
  (with cb (if (null? callback) noop (car callback))
    (dialogue-window wallet-widget-destroy cb "Confirm wallet destruction")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wallet delete entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wallet-entry->string x)
  (cond ((string? x) x)
        ((list? x) (string-recompose (map wallet-entry->string x) ", "))
        ((pair? x) (string-append (wallet-entry->string (car x)) ": "
                                  (wallet-entry->string (cdr x))))
        (else (object->string x))))

(define (ahash-entries t)
  (map car (ahash-table->list t)))

(tm-widget (wallet-widget-delete cmd)
  (with tbl (make-ahash-table)
    (padded
      (resize ("550px" "550px" "9999px") ("250px" "250px" "9999px")
        (refreshable "wallet-widget-delete"
          (scrollable
            (padded
              (for (x (wallet-entries))
                (aligned
		  (item (hlist (toggle
				(if answer
				    (ahash-set! tbl (car x) #t)
				    (ahash-remove! tbl (car x)))
				#f) //)
		    (hlist // // (text (wallet-entry->string x))))))
	      (glue #f #t 0 0))))
        ===
        (bottom-buttons
          >>
          ("Cancel" (cmd "Cancel")) // //
          ("Delete"
           (map wallet-delete (ahash-entries tbl))
           (cmd "Delete")))))))

(tm-define (wallet-dialogue-delete)
  (dialogue-window wallet-widget-delete noop
                   "Delete selected entries in wallet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically remember all passphrases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wallet-always-remember "off")

(define (notify-wallet-always-remember var val)
  (set! wallet-always-remember val))

(define-preferences
  ("wallet always remember" "off" notify-wallet-always-remember))

(tm-define (wallet-always-remember-on?)
  (== (get-preference "wallet always remember") "on"))

(tm-define (wallet-turn-always-remember-on)
  (set-preference "wallet always remember" "on"))

(tm-define (wallet-turn-always-remember-off)
  (set-preference "wallet always remember" "off"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open wallet on demand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (expand-with-wallet body)
  `(if (and (wallet-initialized?)
            (wallet-persistent-status-on?)
            (wallet-off?))
       (wallet-dialogue-turn-on (lambda (x) ,@body))
       (begin ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wallet preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (wallet-not-supported-preferences-widget)
  (centered
    (text "Passphrase wallet facilities are not currently available.")
    (text "Please install GnuPG software from https://www.gnupg.org/,")
    (text "and perform casual settings from the \"Encryption\" tab.")))

(tm-widget (wallet-initialized-preferences-widget)
  (with cb (lambda (x) (refresh-now "security-preferences-refresher"))
    (if (wallet-off?)
        (hlist
          (text "Wallet:") // //
          (explicit-buttons
            ("Turn on" (wallet-dialogue-turn-on cb)) // //
            ("Destroy" (wallet-dialogue-destroy cb)) >>)))
    (if (wallet-on?)
        (hlist
          (text "Wallet:") // //
          (explicit-buttons
            ("Turn off" (begin (wallet-turn-off) (cb "Ok"))) // //
            ("Delete entries" (wallet-dialogue-delete)) // //
            ("Change passphrase"
             (wallet-dialogue-reinitialize cb)) >>)))
    ===
    (hlist
      (text "Automatically remember all passphrases in the wallet?") // //
      (enum (if (== answer "yes")
                (wallet-turn-always-remember-on)
                (wallet-turn-always-remember-off))
            (list "yes" "no")
            (if (wallet-always-remember-on?) "yes" "no")
            "4em") >>)))

(tm-widget (wallet-uninitialized-preferences-widget)
  (with cb (lambda (x) (refresh-now "security-preferences-refresher"))
    (hlist
      (text "Wallet:") // //
      (explicit-buttons
        ("Initialize" (wallet-dialogue-initialize cb))) >>)))

(tm-widget (wallet-preferences-widget)
  (if (and (supports-wallet?) (wallet-initialized?))
      (dynamic (wallet-initialized-preferences-widget)))
  (if (and (supports-wallet?) (not (wallet-initialized?)))
      (dynamic (wallet-uninitialized-preferences-widget)))
  (if (not (supports-wallet?))
    (dynamic (wallet-not-supported-preferences-widget))))
