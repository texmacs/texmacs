
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macos-security.scm
;; DESCRIPTION : Interface to Mac OS 'security' command
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils keychain macos-security))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (system-security-error cmd out err)
  (report-system-error "Mac OS security command failed" cmd out err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (system-security-add-generic-password-command
	 account service password)
  (string-append "add-generic-password"
		 " -a " (string-quote account)
		 " -s " (string-quote service)
		 " -p " (string-quote password) "\n"))

(tm-define (system-security-add-generic-password account service password)
  (let* ((cmd (system-security-add-generic-password-command
	       account service password))
         (pcm (system-security-add-generic-password-command
	       account service "********"))
         (ret (evaluate-system (list "security" "-i")
                               '(0) (list cmd) '(1 2))))
    (if (string<> (car ret) "0")
      (system-security-error (list pcm) (cadr ret) (caddr ret))
      #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (system-security-find-generic-password-command account service)
  (string-append "find-generic-password"
		 " -a " (string-quote account)
		 " -s " (string-quote service)
		 " -w " "\n"))

(tm-define (system-security-find-generic-password account service)
  (with cmd (system-security-find-generic-password-command account service)
    (with ret (evaluate-system (list "security" "-i") '(0) (list cmd) '(1 2))
      (if (string<> (car ret) "0")
	  (system-security-error (list cmd) (cadr ret) (caddr ret))
	  (car (string-decompose (cadr ret) "\n"))))))

(tm-define (system-security-quiet-find-generic-password account service)
  (with cmd (system-security-find-generic-password-command account service)
    (with ret (evaluate-system (list "security" "-i") '(0) (list cmd) '(1 2))
      (if (string<> (car ret) "0")
	  #f
	  (car (string-decompose (cadr ret) "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (system-security-delete-generic-password-command account service)
  (string-append "delete-generic-password"
		 " -a " (string-quote account)
		 " -s " (string-quote service) "\n"))

(tm-define (system-security-delete-generic-password account service)
  (with cmd (system-security-delete-generic-password-command account service)
    (with ret (evaluate-system (list "security" "-i") '(0) (list cmd) '(1 2))
      (if (string<> (car ret) "0")
	  (system-security-error (list cmd) (cadr ret) (caddr ret))
	  #t))))
