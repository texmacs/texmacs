
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-preferences.scm
;; DESCRIPTION : management of the user preferences
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-preferences)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining preference call back routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public preferences-initialization-flag #f)
(define-public preferences-table (make-ahash-table))
(define-public preferences-call-back (make-ahash-table))

(define (define-preference x)
  (with (which value call-back) x
    `(if (not (ahash-ref preferences-table ,which))
	 (ahash-set! preferences-table ,which ,value))))

(define (define-preference-call-back x)
  (with (which value call-back) x
    `(begin
       (ahash-set! preferences-call-back ,which ,call-back)
       (if preferences-initialization-flag (notify-preference ,which)))))

(define-public-macro (define-preferences . l)
  (append '(begin)
	  (map-in-order define-preference l)
	  (map-in-order define-preference-call-back l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting and getting preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-preference? which what)
  (== what (get-preference which)))

(tm-define (set-preference which what)
  (:synopsis "Set preference @which to @what")
  (:check-mark "*" test-preference?)
  (ahash-set! preferences-table which what)
  ;(display* "set-preference " which " := " what "\n")
  ((get-call-back which) which what)
  (save-preferences))

(tm-define (get-preference which)
  (:synopsis "Get preference @which")
  (ahash-ref preferences-table which))

(define (preference-on? which)
  (test-preference? which "on"))

(tm-define (toggle-preference which)
  (:synopsis "Toggle the preference @which")
  (:check-mark "v" preference-on?)
  (let ((what (get-preference which)))
    (set-preference which (cond ((== what "on") "off")
				((== what "off") "on")
				(else what)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-call-back what)
  (let ((r (ahash-ref preferences-call-back what)))
    (if r r (lambda args (noop)))))

(define-public (notify-preference var)
  "Notify a change in preference @var"
  ;(display* "notify-preference " var "\n")
  ((get-call-back var) var (get-preference var)))

(define (preference-apply l)
  ;(display* "preference-apply " l "\n")
  ((get-call-back (car l)) (car l) (cadr l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize preferences and consulting preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define user-preferences '())
(define saved-preferences '())

(define (preferences->list table)
  (let* ((folder (lambda (key im tail) (cons (list key im) tail)))
	 (unsorted (ahash-fold folder '() table))
	 (comp? (lambda (l1 l2) (string<=? (car l1) (car l2)))))
    (list-sort unsorted comp?)))

(define (save-preferences)
  (set! user-preferences (preferences->list preferences-table))
  (if (!= user-preferences saved-preferences)
      (begin
	(save-object "$TEXMACS_HOME_PATH/system/preferences.scm"
		     user-preferences)
	(set! saved-preferences user-preferences))))

(define-public (retrieve-preferences)
  "Retrieve preferences from disk"
  (if (url-exists? "$TEXMACS_HOME_PATH/system/preferences.scm")
      (set! saved-preferences
	    (load-object "$TEXMACS_HOME_PATH/system/preferences.scm")))
  (fill-dictionary preferences-table saved-preferences))

(define-public (apply-preferences)
  "Apply the preferences"
  (import-from (texmacs keyboard config-kbd) (texmacs texmacs tm-server)
	       (texmacs texmacs tm-view) (texmacs texmacs tm-print)
	       (utils edit brackets-edit))
  (map-in-order preference-apply (preferences->list preferences-table))
  (set! preferences-initialization-flag #t))
