
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : win-security.scm
;; DESCRIPTION : Interface to Mac OS security software
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf, Denis Raux
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils macos-security win-security-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (os-security-error cmd out err)
  (report-system-error "Windows security command failed" cmd out err))

;; WAllet command name
(define wallet-cmd (url-concretize "$TEXMACS_PATH\\bin\\winwallet.exe"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (os-security-add-generic-password account service password)
  (with ret (evaluate-system (list wallet-cmd "ADD" account service)
			     '(0) (list password) '(1 2))
    (== (car ret) "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (os-security-find-generic-password account service)
  (with ret (evaluate-system (list wallet-cmd "GET" account service)
			     '(0) (list "") '(1 2))
    (if (!= (car ret) "0")
	(os-security-error (list wallet-cmd "GET" account service)
			   (cadr ret) (caddr ret))
	(car (string-decompose (cadr ret) "\n")))))

(tm-define (os-security-quiet-find-generic-password account service)
  (with ret (evaluate-system (list wallet-cmd "GET" account service)
			     '(0) (list "")  '(1 2))
    (and (== (car ret) "0")
	 (car (string-decompose (cadr ret) "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (os-security-delete-generic-password account service)
  (with ret (evaluate-system (list wallet-cmd "RM" account service)
			     '(0) (list "") '(1 2))
    (or (== (car ret) "0")
	(os-security-error (list cmd) (cadr ret) (caddr ret)))))
