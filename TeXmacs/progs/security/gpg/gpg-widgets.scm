
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gpg-widgets.scm
;; DESCRIPTION : wallet
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security gpg gpg-widgets)
  (:use (security gpg gpg-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG preferences
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
          "" "15em") // //
          (explicit-buttons
            ("Browse"
             (choose-file (lambda (x)
                            (gpg-set-executable (url->system x))
                            (refresh-now "security-preferences-refresher"))
                          "Select GnuPG command" ""))) >>))

(tm-widget (gpg-supported-preferences-widget)
  (hlist
    (text "GnuPG command:") // //
    (enum (gpg-set-executable answer)
          (list (gpg-get-executable) "gpg" "gpg2" "")
          (gpg-get-executable) "15em") // //
          (explicit-buttons
            ("Browse"
             (choose-file (lambda (x)
                            (gpg-set-executable (url->system x))
                            (refresh-now "security-preferences-refresher"))
                          "Select GnuPG command" ""))) >>))

(tm-widget (gpg-preferences-widget)
  (if (supports-gpg?)
      (dynamic (gpg-supported-preferences-widget)))
  (if (not (supports-gpg?))
      (dynamic (gpg-not-supported-preferences-widget))))

