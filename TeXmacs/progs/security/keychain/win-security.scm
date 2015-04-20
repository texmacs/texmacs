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
(define os-security-error-widget-cmd "")
(define os-security-error-widget-out "")
(define os-security-error-widget-err "")

(tm-widget (os-security-error-widget cmd)
  (resize ("400px" "800px" "800px") ("400px" "400px" "400px")
  (centered (bold (text "Input command")))  
  (scrollable
    (for (x (string-decompose os-security-error-widget-cmd "\n")) (text x)))
  ===
  (centered (bold (text "Standard Output")))
  (scrollable
    (for (x (string-decompose os-security-error-widget-out "\n")) (text x)))
  ===
  (centered (bold (text "Error output")))
  (scrollable
    (for (x (string-decompose os-security-error-widget-err "\n")) (text x)))
  ===
  (bottom-buttons >> ("Ok" (cmd)))))

(define (os-security-error cmd out err)
  (set! os-security-error-widget-cmd (string-recompose cmd " "))
  (set! os-security-error-widget-out (utf8->cork out))
  (set! os-security-error-widget-err (utf8->cork err))
  (dialogue-window os-security-error-widget noop
		   "Windows security command failed")
  #f)
;; WAllet command name

  (define walletcmd (url-concretize "$TEXMACS_PATH\\bin\\winwallet.exe"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (os-security-add-generic-password account service password)
  (with ret (evaluate-system (list walletcmd "ADD" account service)
                               '(0) (list password) '(1 2))
    (string= (car ret) "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(tm-define (os-security-find-generic-password account service)
  (with ret (evaluate-system (list walletcmd  "GET" account service) '(0) (list "") '(1 2))
    (if (string<> (car ret) "0")
      (os-security-error (list walletcmd "GET"  account service) (cadr ret) (caddr ret))
      (car (string-decompose (cadr ret) "\n")))))

(tm-define (os-security-quiet-find-generic-password account service)
  (with ret (evaluate-system (list walletcmd "GET" account service) '(0) (list "")  '(1 2))
    (if (string<> (car ret) "0")
      #f
      (car (string-decompose (cadr ret) "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete generic password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (os-security-delete-generic-password account service)
  (with ret (evaluate-system (list walletcmd "RM" account service) '(0) (list "") '(1 2))
    (if (string<> (car ret) "0")
      (os-security-error (list cmd) (cadr ret) (caddr ret))
      #t)))
