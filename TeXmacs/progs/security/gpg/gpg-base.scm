
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gpg-base.scm
;; DESCRIPTION : GnuPG interface
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security gpg gpg-base)
  (:use (utils base environment)))

;(display "TeXmacs] Loading GnuPG base support (http://www.gnupg.org)\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpg-executable "")

(define gpg-executable-version-table (make-ahash-table))

(define (gpg-valid-executable? exe)
  (let* ((x (ahash-ref gpg-executable-version-table exe))
         (tmp ""))
    (if x (string<> x "no")
        (with b (and
                 (string<> exe "")
                 (url-exists-in-path? exe)
                 (with ret (evaluate-system (list exe "--version")
                                            '() '() '(1 2))
                   (set! tmp (cadr ret))
                   (and (string= (car ret) "0")
                        (string-contains? tmp "GnuPG"))))
          (if b
              (with aux (string-decompose (car (string-decompose tmp "\n")) " ")
                (with ver (if (>= (length aux) 3) (third aux) "no")
                  (ahash-set! gpg-executable-version-table exe ver)
                  (string<> ver "no")))
              (begin
                (ahash-set! gpg-executable-version-table exe "no")
                #f))))))

(define (notify-gpg-executable var val)
  (set! gpg-executable val))

(define-preferences
  ("gpg executable" (if (url-exists-in-path? "gpg") "gpg" "gpg2")
   notify-gpg-executable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG file format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format gpg
  (:name "GnuPG")
  (:suffix "gpg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global settings and initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpg-homedir
  (url-concretize "$TEXMACS_HOME_PATH/system/gnupg"))

(define (gpg-make-homedir)
  (when (not (url-exists? gpg-homedir))
    (system-mkdir gpg-homedir)
    (when (not (or (os-mingw?) (os-win32?)))
      (system-1 "chmod og-rwx" gpg-homedir))))

(when (gpg-valid-executable? gpg-executable)
  (gpg-make-homedir))

(tm-define (gpg-get-executable)
  (:synopsis "GnuPG executable")
  gpg-executable)

(tm-define (gpg-set-executable exe)
  (:interactive #t)
  (:synopsis "Set GnuPG executable")
  (:argument exe "GnuPG executable")
  (when (gpg-valid-executable? exe)
    (gpg-make-homedir)
    (set-preference "gpg executable" exe)))

(tm-define (supports-gpg?)
  (:synopsis "Tells if GnuPG is available")
  (and (gpg-valid-executable? gpg-executable)
       (url-exists? gpg-homedir)))

(tm-define (gpg-version)
  (string-decompose
    (ahash-ref gpg-executable-version-table gpg-executable) "."))

(tm-define (gpg-major-version-number)
  (with ver (gpg-version)
    (if (not (null? ver)) (car ver) "?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-error cmd out err)
  (report-system-error "GnuPG command failed" cmd out err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common arguments for batch mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-default homedir)
  (if (url-none? homedir)
      (list gpg-executable "--homedir" (url->system gpg-homedir)
            "--batch" "--no-tty" "--no-use-agent")
      (list gpg-executable "--homedir" (url->system homedir)
            "--batch" "--no-tty" "--no-use-agent")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create new public secret key pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-gen-key-make-formatted-input name email comment passphrase)
  (with opt-comment (if (string<> comment "")
			(string-append "Name-Comment: " comment "\n") "")
	(string-append "Key-Type: RSA\n"
		       "Key-Length: 4096\n"
		       "Name-Real: " name "\n"
		       opt-comment
		       "Name-email: " email "\n"
		       "Passphrase: " passphrase "\n"
		       "%commit" "\n"
		       "%echo ok" "\n")))

(define (gpg-executable-gen-key homedir)
  (append (gpg-executable-default homedir) (list "--gen-key" "-")))

(tm-define (gpg-gen-key name email comment passphrase . homedir)
  (:synopsis "Create a new GnuGP identity")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-gen-key dir)
  (with ret (evaluate-system cmd
	      '(0) (list (gpg-gen-key-make-formatted-input
			   name email comment passphrase)) '(2 3))
	(if (string<> (car ret) "0")
	    (gpg-error cmd (cadr ret) (caddr ret)) #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List public keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-decompose-key-string s) (string-decompose s ":"))
(define (gpg-public-key-row? row) (string= (car row) "pub"))
(define (gpg-secret-key-row? row) (string= (car row) "sec"))
(define (gpg-fingerprint-row? row) (string= (car row) "fpr"))
(define (gpg-user-id-row? row) (string= (car row) "uid"))
(define (gpg-data-row? row) (string= (car row) "pkd"))

(define (gpg-get-first-public-key-tail l)
  (cond ((null? l) l)
	((gpg-public-key-row? (car l)) '())
	(else (cons (car l) (gpg-get-first-public-key-tail (cdr l))))))

(define (gpg-get-first-public-key l)
  (if (null? l) l
      (cons (car l) (gpg-get-first-public-key-tail (cdr l))))) 

(define (gpg-split-public-key-list l)
  (if (or (null? l) (null? (car l))) l
      (if (gpg-public-key-row? (car l))
	  (cons (gpg-get-first-public-key l)
		(gpg-split-public-key-list (cdr l)))
	  (gpg-split-public-key-list (cdr l)))))

(define (gpg-executable-list-public-keys homedir)
  (append (gpg-executable-default homedir)
	  (list "--list-public-keys" "--with-fingerprint"
		"--with-key-data" "--with-colons" "--fixed-list-mode")))

(tm-define (gpg-public-keys . homedir)
  (:synopsis "GnuPG public keys")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-list-public-keys dir)
  (with ret (evaluate-system cmd '() '() '(1 2))
  (if (string<> (car ret) "0")
      (gpg-error cmd (cadr ret) (caddr ret))
      (let* ((srows (string-decompose (utf8->cork (cadr ret)) "\n"))
	     (crows (filter (lambda (x) (string<> x "")) srows))
	     (rows (map gpg-decompose-key-string crows)))
	(gpg-split-public-key-list rows)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List secret keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-get-first-secret-key-tail l)
  (cond ((null? l) l)
	((gpg-secret-key-row? (car l)) '())
	(else (cons (car l) (gpg-get-first-secret-key-tail (cdr l)))))) 

(define (gpg-get-first-secret-key l)
  (if (null? l) l
      (cons (car l) (gpg-get-first-secret-key-tail (cdr l))))) 

(define (gpg-split-secret-key-list l)
  (if (or (null? l) (null? (car l))) l
      (if (gpg-secret-key-row? (car l))
	  (cons (gpg-get-first-secret-key l)
		(gpg-split-secret-key-list (cdr l)))
	  (gpg-split-secret-key-list (cdr l)))))

(define (gpg-executable-list-secret-keys homedir)
  (append (gpg-executable-default homedir)
    (list "--list-secret-keys" "--with-fingerprint"
	  "--with-colons" "--fixed-list-mode")))

(tm-define (gpg-secret-keys . homedir)
  (:synopsis "GnuPG secret keys")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-list-secret-keys dir)
  (with ret (evaluate-system cmd '() '() '(1 2))
  (if (string<> (car ret) "0")
      (gpg-error cmd (cadr ret) (caddr ret))
      (let* ((srows (string-decompose (utf8->cork (cadr ret)) "\n"))
	     (crows (filter (lambda (x) (string<> x "")) srows))
	     (rows (map gpg-decompose-key-string crows)))
        (gpg-split-secret-key-list rows)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-public-key-fingerprints . homedir)
  (:synopsis "List the fingerprints of the public keyring")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with keys (gpg-public-keys dir)
    (map (lambda (x) (tenth (car (filter gpg-fingerprint-row? x)))) keys))))

(tm-define (gpg-public-key-fingerprint? fpr . homedir)
  (:synopsis "Tells if @fpr actually belongs to the public keyring")
  (with dir (if (null? homedir) (url-none) (car homedir)) 
  (member fpr (gpg-public-key-fingerprints dir))))

(tm-define (gpg-secret-key-fingerprints . homedir)
  (:synopsis "List the fingerprints of the secret keyring")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with keys (gpg-secret-keys dir)
    (map (lambda (x) (tenth (car (filter gpg-fingerprint-row? x)))) keys))))

(tm-define (gpg-secret-key-fingerprint? fpr . homedir)
  (:synopsis "Tells if @fpr actually belongs to the secret keyring")
  (with dir (if (null? homedir) (url-none) (car homedir)) 
  (member fpr (gpg-secret-key-fingerprints dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access to key infomations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-get-key-fingerprint key)
  (tenth (car (filter gpg-fingerprint-row? key))))

(tm-define (gpg-get-key-user-id key)
  (tenth (car (filter gpg-user-id-row? key))))

(tm-define (gpg-get-key-data key)
  (gpg-export-public-keys (list (gpg-get-key-fingerprint key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-key-search-by-fingerprint fpr keys)
  (with f (filter (lambda (x) (string= (gpg-get-key-fingerprint x) fpr)) keys)
    (if (null? f) #f (car f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete public key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-delete-public-key fingerprint homedir)
  (append (gpg-executable-default homedir)
    (list "--quiet" "--yes" "--delete-public-key" fingerprint)))

(tm-define (gpg-delete-public-key fingerprint . homedir)
  (:synopsis "Delete GnuPG public key of fingerprint @fingerprint")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-delete-secret-and-public-key fingerprint dir)
    (with ret (evaluate-system cmd '() '() '(1 2))
      (if (string= (car ret) "0") #t
        (gpg-error cmd (cadr ret) (caddr ret)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete secret and public keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-delete-secret-and-public-key fingerprint homedir)
  (append (gpg-executable-default homedir)
    (list "--quiet" "--yes" "--delete-secret-and-public-key" fingerprint)))

(tm-define (gpg-delete-secret-and-public-key fingerprint . homedir)
  (:synopsis "Delete GnuPG secret and public keys of fingerprint @fingerprint")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-delete-secret-and-public-key fingerprint dir)
    (with ret (evaluate-system cmd '() '() '(1 2))
      (if (string= (car ret) "0") #t
        (gpg-error cmd (cadr ret) (caddr ret)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-encrypt rcps homedir)
  (append (gpg-executable-default homedir)
    (list "--encrypt") rcps
    (list "--trust-model" "always")
    (list "--armor" "--batch" "--no-tty" "-")))

(tm-define (gpg-encrypt data rcps . homedir)
  (:synopsis "GnuPG encrypt string @data for recipient fingerprint list @rcps") 
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with rcps-args (map (lambda (x) (string-append "-r " x)) rcps)
  (with cmd (gpg-executable-encrypt rcps-args dir)
  (with ret (evaluate-system cmd '(0) (list data) '(1 2))
    (if (string<> (car ret) "0")
	(gpg-error cmd (cadr ret) (caddr ret))
	(cadr ret)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-decrypt homedir)
  (append (gpg-executable-default homedir)
    (if (or (os-mingw?) (os-win32?))
      (list "--decrypt" "--passphrase-fd" "$%1" "--armor")
      (list "--decrypt" "--passphrase-fd" "$$1" "--armor"))))

(tm-define (gpg-decrypt data passphrase . homedir)
  (:synopsis "GnuPG decrypt armored string @data with passphrase @passphrase") 
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-decrypt dir)
    (with ret (evaluate-system cmd '(0 -1) (list data passphrase) '(1 2))
      (if (string<> (car ret) "0")
        (gpg-error cmd (cadr ret) (caddr ret))
        (cadr ret))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-decryptable? data passphrase . homedir)
  (:synopsis "Tells if @data can be decrypted with passphrase @passphrase")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with cmd (gpg-executable-decrypt dir)
    (with ret (evaluate-system cmd '(0 -1) (list data passphrase) '(1 2))
      (string= (car ret) "0")))))

(tm-define (gpg-correct-passphrase? fingerprint passphrase . homedir)
  (:synopsis "Tells if passphrase @passphrase is correct for @fingerprint")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with enc (gpg-encrypt "test" (list fingerprint) dir)
  (and enc (gpg-decryptable? enc passphrase dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export public keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-export-public-keys fingerprints homedir)
  (append (gpg-executable-default homedir)
    (list  "--armor" "--export") fingerprints))

(tm-define (gpg-export-public-keys fingerprints . homedir)
  (:synopsis "Export GnuPG public keys of fingerprint in the given list")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with fingerprints-args
    (map (lambda (x) (string-append " " x)) fingerprints)
  (with cmd (gpg-executable-export-public-keys fingerprints-args dir)
    (with ret (evaluate-system cmd '() '() '(1 2))
      (if (string<> (car ret) "0")
        (gpg-error cmd (cadr ret) (caddr ret))
        (cadr ret)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export secret keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-export-secret-keys fingerprints homedir)
  (append (gpg-executable-default homedir)
    (list  "--armor" "--export-secret-keys") fingerprints))

(tm-define (gpg-export-secret-keys fingerprints . homedir)
  (:synopsis "Export GnuPG secret keys of fingerprint in the given list")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with fingerprints-args
    (map (lambda (x) (string-append " " x)) fingerprints)
  (with cmd (gpg-executable-export-secret-keys fingerprints-args dir)
    (with ret (evaluate-system cmd '() '() '(1 2))
      (if (string<> (car ret) "0")
        (gpg-error cmd (cadr ret) (caddr ret))
        (cadr ret)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gpg-executable-import homedir)
  (append (gpg-executable-default homedir)
    (list  "--armor" "--import" "-")))

(define (gpg-import s homedir)
  (with cmd (gpg-executable-import homedir)
  (with ret (evaluate-system cmd '(0) (list s) '(1 2))
    (if (string<> (car ret) "0")
        (gpg-error cmd (cadr ret) (caddr ret))
        #t))))

(tm-define (gpg-import-public-keys s . homedir)
  (:synopsis "Import GnuPG public keys")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (gpg-import s dir)))

(tm-define (gpg-import-secret-keys s . homedir)
  (:synopsis "Import GnuPG secret keys")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (gpg-import s dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File save and load encrypted for a single key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gpg-string-encrypt-save s url fingerprint . homedir)
  (:synopsis "Encrypt and save string @s to @url for @fingerprint")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with e (gpg-encrypt s (list fingerprint) dir)
    (if e (string-save e url) #f))))

(tm-define (gpg-string-load-decrypt url passphrase . homedir)
  (:synopsis "Load and decrypt string from @url with @passphrase")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with s (string-load url)
    (if s (gpg-decrypt s passphrase dir) #f))))

(tm-define (gpg-encrypt-save-object url o fingerprint . homedir)
  (:synopsis "Encrypt and save object @o to @url for @fingerprint")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with port (open-output-string)
    (pretty-print o port)
    (with ret
      (gpg-string-encrypt-save (get-output-string port) url fingerprint dir)
      (close-output-port port)
      ret))))

(tm-define (gpg-load-decrypt-object url passphrase . homedir)
  (:synopsis "Load and decrypt object from @url with @passphrase")
  (with dir (if (null? homedir) (url-none) (car homedir))
  (with d (gpg-string-load-decrypt url passphrase dir)
    (if d (with p (open-input-string d)
	  (with e (read p)
	  (if (eof-object? e) '() e))) #f))))
