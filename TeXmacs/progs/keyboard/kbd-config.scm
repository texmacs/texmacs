
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-config.scm
;; DESCRIPTION : keyboard configuration
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (keyboard kbd-config)
  (:export compute-wildcard-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefix modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prefix-modifier-table (make-ahash-table))

(define (get-default-alt-modifier)
  (let ((s (eval-system "xmodmap | grep Alt_L")))
    (cond ((string-starts? s "mod")
	   (string-append "Mod" (substring s 3 4)))
	  (else "Mod1"))))

(define (get-default-meta-modifier)
  (let ((s (eval-system "xmodmap | grep Meta_L")))
    (cond ((string-starts? s "mod")
	   (string-append "Mod" (substring s 3 4)))
	  (else (get-default-super-modifier)))))

(define (get-default-super-modifier)
  (let ((s (eval-system "xmodmap | grep Super_L")))
    (cond ((string-starts? s "mod")
	   (string-append "Mod" (substring s 3 4)))
	  (else "Mod4"))))

(define (get-default-hyper-modifier)
  (let ((s (eval-system "xmodmap | grep Hyper_L")))
    (cond ((string-starts? s "mod")
	   (string-append "Mod" (substring s 3 4)))
	  (else "Mod3"))))

(define (notify-prefix-modifier var val)
  (cond ((not (== val "default"))
	 (ahash-set! prefix-modifier-table var val)
	 (if preferences-initialization-flag
	     (set-message
	      "Restart in order to let the new look and feel take effect"
	      "configure look and feel")))
	((== var "A")
	 (notify-prefix-modifier var (get-default-alt-modifier)))
	((== var "M")
	 (notify-prefix-modifier var (get-default-meta-modifier)))
	((== var "H")
	 (notify-prefix-modifier var (get-default-hyper-modifier)))))

(define-preferences
  ("A" "default" notify-prefix-modifier)
  ("M" "default" notify-prefix-modifier)
  ("H" "default" notify-prefix-modifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefix modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-prefix-modifier s)
  (ahash-ref prefix-modifier-table s))

(define (get-prefix-modifiers l)
  (list-sort (map-in-order get-prefix-modifier l) string>?))

(define (modifier-list->string l)
  (apply string-append (map-in-order (lambda (s) (string-append s "-")) l)))

(define (compute-wildcard-line l)
  (let* ((r (get-prefix-modifiers l))
	 (L (modifier-list->string l))
	 (R (modifier-list->string r)))
    (list R L #t)))

(define (compute-wildcard-lines)
  (map-in-order compute-wildcard-line
   '(("A")
     ("M")
     ("H")
     ("M" "A")
     ("H" "A")
     ("H" "M")
     ("H" "M" "A"))))

(define (xmodmap s)
  (system (string-append "xmodmap -e \"" s "\"")))

(define (notify-key-prefix var val)
  (let* ((Modx (get-prefix-modifier val)))
    ;(display* var " -> " val " -> " Modx "\n")
    (cond ((== Modx #f) (noop))
	  ((== var "alt")
	   (xmodmap (string-append "clear " Modx))
	   (xmodmap (string-append "add " Modx " = Alt_L"))
	   (xmodmap (string-append "add " Modx " = Alt_R")))
	  ((== var "meta")
	   (xmodmap (string-append "clear " Modx))
	   (xmodmap (string-append "add " Modx " = Meta_L"))
	   (xmodmap (string-append "add " Modx " = Meta_R")))
	  ((== var "windows")
	   (xmodmap "keycode 115 = Super_L")
	   (xmodmap "keycode 116 = Super_R")
	   (xmodmap (string-append "clear " Modx))
	   (xmodmap (string-append "add " Modx " = Super_L"))
	   (xmodmap (string-append "add " Modx " = Super_R")))
	  ((== var "caps-lock")
	   (xmodmap "clear Lock")
	   (xmodmap (string-append "clear " Modx))
	   (xmodmap (string-append "add " Modx " = Caps_Lock"))))))

(define-preferences
  ("alt" "default" notify-key-prefix)
  ("meta" "default" notify-key-prefix)
  ("windows" "default" notify-key-prefix)
  ("caps-lock" "default" notify-key-prefix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cyrillic input method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-cyrillic-input-method var val)
  (cond ((== val "translit") (import-from (keyboard cyrillic kbd-translit)))
	((== val "jcuken") (import-from (keyboard cyrillic kbd-jcuken)))
	((== val "yawerty") (import-from (keyboard cyrillic kbd-yawerty)))
	((== val "koi8-r") (import-from (keyboard cyrillic kbd-koi8)))
	((== val "cp1251") (import-from (keyboard cyrillic kbd-cp1251)))))

(define-preferences
  ("cyrillic input method" "" notify-cyrillic-input-method))
