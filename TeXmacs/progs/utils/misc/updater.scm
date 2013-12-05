
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : updater.scm
;; DESCRIPTION : support utilities for tm_updater
;; COPYRIGHT   : (C) 2013 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc updater))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string->bool s)
  (and (string? s)
       (or (== s "#t") (== s "true") (== s "on"))))

(tm-define (bool->string b)
  (if b "#t" "#f"))

(tm-define (updater-initialize)
   (let ((appcast  (get-preference "updater:appcast"))
         (interval (string->number (get-preference "updater:interval")))
         (auto?    (string->bool (get-preference "updater:auto"))))
     (updater-set-appcast appcast)
     (updater-set-interval interval)
     (updater-set-automatic auto?)
     (if auto? (updater-check-background))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preference management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (updater-notify-pref pref val)
  (cond ((== pref "updater:appcast") (updater-set-appcast val))
        ((== pref "updater:public-dsa-key") (noop))
        ((== pref "updater:interval")
         (with n (string->number val)
           (if (< n 24)
               (set-preference "updater:automatic-checks" #f)
               (begin 
                 (updater-set-interval n)
                 (set-preference "updater:automatic-checks" #t)))))
        ((== pref "updater:automatic-checks") 
         (updater-set-automatic (string->bool val)))
        (else (noop))))

(define-preferences
  ("updater:appcast" "http://www.texmacs.org/Download/ftp/tmftp/appcast.xml"
                     updater-notify-pref)
  ; dsa key ignored by Sparkle (set in Info.plist), unused by WinSparkle
  ("updater:public-dsa-key" "texmacs_updates_dsa_pub.pem" updater-notify-pref)
  ("updater:interval" "24" updater-notify-pref)
  ("updater:automatic-checks" 
   (bool->string (or (os-win32?) (os-mingw?) (os-macos?)))
   updater-notify-pref))
