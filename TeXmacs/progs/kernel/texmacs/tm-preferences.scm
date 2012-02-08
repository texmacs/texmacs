
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-preferences.scm
;; DESCRIPTION : management of the user preferences
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-preferences)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining preference call back routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public preferences-table (make-ahash-table))
(define-public preferences-default (make-ahash-table))
(define-public preferences-call-back (make-ahash-table))

(define (define-preference x)
  (with (which value call-back) x
    `(if (not (ahash-ref preferences-default ,which))
	 (ahash-set! preferences-default ,which ,value))))

(define (define-preference-call-back x)
  (with (which value call-back) x
    `(begin
       (ahash-set! preferences-call-back ,which ,call-back)
       (notify-preference ,which))))

(define-public-macro (define-preferences . l)
  (append '(begin)
	  (map-in-order define-preference l)
	  (map-in-order define-preference-call-back l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting and getting preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-preference? which what)
  (== (get-preference which)
      (if (!= what "default") what
          (ahash-ref preferences-default which))))

(tm-define (set-preference which what)
  (:synopsis "Set preference @which to @what")
  (:check-mark "*" test-preference?)
  (if (== what "default")
      (reset-preference which)
      (begin
        (ahash-set! preferences-table which what)
        ;;(display* "set-preference " which " := " what "\n")
        ((get-call-back which) which (get-preference which))
        (save-preferences))))

(tm-define (reset-preference which)
  (:synopsis "Revert preference @which to default setting")
  (ahash-remove! preferences-table which)
  ((get-call-back which) which (get-preference which))
  (save-preferences))

(tm-define (get-preference which)
  (:synopsis "Get preference @which")
  (if (ahash-ref preferences-table which)
      (ahash-ref preferences-table which)
      (ahash-ref preferences-default which)))

(define (preference-on? which)
  (test-preference? which "on"))

(tm-define (toggle-preference which)
  (:synopsis "Toggle the preference @which")
  (:check-mark "v" preference-on?)
  (with what (get-preference which)
    (set-preference which (cond ((== what "on") "off")
				((== what "off") "on")
				(else what)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nicer names for preference values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public preference-decode-table (make-ahash-table))
(define-public preference-encode-table (make-ahash-table))

(tm-define (set-pretty-preference which pretty-val)
  (with val (ahash-ref preference-decode-table (cons which pretty-val))
    (set-preference which (or val pretty-val))))

(tm-define (get-pretty-preference which)
  (with val (get-preference which)
    (with pretty-val (ahash-ref preference-encode-table (cons which val))
      ;;(display* "Get: " which ", " val " -> " pretty-val "\n")
      (or pretty-val val "Default"))))

(tm-define (set-boolean-preference which val)
  (set-preference which (if val "on" "off")))

(tm-define (get-boolean-preference which)
  (== (get-preference which) "on"))

(define-public (set-preference-encode which x)
   `(ahash-set! preference-encode-table
                (cons ,which ,(car x)) ,(cadr x)))

(define-public (set-preference-decode which x)
   `(ahash-set! preference-decode-table
                (cons ,which ,(cadr x)) ,(car x)))

(define-public-macro (define-preference-names which . l)
  `(begin
     ,@(map (lambda (x) (set-preference-encode which x)) l)
     ,@(map (lambda (x) (set-preference-decode which x)) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (look-and-feel)
  (with s (get-preference "look and feel")
    (if (== s "default") (default-look-and-feel) s)))

(define (test-look-and-feel t)
  ;;(display* "Check look and feel " t "\n")
  (cond ((list? t) (list-or (map test-look-and-feel t)))
	((symbol? t) (test-look-and-feel (symbol->string t)))
	(else
	  (with s (look-and-feel)
	    (or (== t s) (and (== t "std") (!= s "emacs")))))))

(set! has-look-and-feel? test-look-and-feel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-call-back what)
  (let ((r (ahash-ref preferences-call-back what)))
    (if r r (lambda args (noop)))))

(define-public (notify-preference var)
  "Notify a change in preference @var"
  ;;(display* "notify-preference " var ", " (get-preference var) "\n")
  ((get-call-back var) var (get-preference var)))

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

(define (retrieve-preferences)
  "Retrieve preferences from disk"
  (if (url-exists? "$TEXMACS_HOME_PATH/system/preferences.scm")
      (set! saved-preferences
	    (load-object "$TEXMACS_HOME_PATH/system/preferences.scm")))
  (fill-dictionary preferences-table saved-preferences))

(retrieve-preferences)
(notify-preferences-loaded)
