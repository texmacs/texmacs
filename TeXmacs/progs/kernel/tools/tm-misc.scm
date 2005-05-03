
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-misc.scm
;; DESCRIPTION : important miscellaneous subroutines
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel tools tm-misc)
  (:use (kernel texmacs tm-define) (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtrees and path rounding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tm-start p)
  (:type (-> path path))
  (:synopsis "Round cursor position @p to below.")
  (cursor-start (the-root) p))

(tm-define (tm-end p)
  (:type (-> path path))
  (:synopsis "Round cursor position @p to above.")
  (cursor-end (the-root) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (test-default? . vals)
  (if (null? vals)
      #t
      (and (not (init-has? (car vals)))
	   (apply test-default? (cdr vals)))))

(tm-define (init-default . args)
  (:check-mark "*" test-default?)
  (for-each init-default-one args))

(define-public (test-init? var val)
  (== (get-init-tree var) (string->tree val)))

(tm-property (init-env var val)
  (:check-mark "*" test-init?))

(define-public (test-env? var val)
  (== (get-env var) val))

(tm-property (make-with var val)
  (:check-mark "o" test-env?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading general objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (save-object file value)
  (write value (open-file (url-materialize file "") OPEN_WRITE))
  (flush-all-ports))

(define-public (load-object file)
  (read (open-file (url-materialize file "r") OPEN_READ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (not-implemented s)
  (set-message "Error: not yet implemented" s))

(tm-define (tm-debug)
  (:type (-> void))
  (:synopsis "For debugging purposes.")
  (display* (tree->stree (the-root)) "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (texmacs-version-release* t)
  (texmacs-version-release (tree->string t)))

(define-public (real-math-font? fn)
  (or (== fn "roman") (== fn "concrete")))

(define-public (real-math-family? fn)
  (or (== fn "mr") (== fn "ms") (== fn "mt")))

(define-public (kill-line)
  (selection-set-start)
  (go-end-line)
  (selection-set-end)
  (clipboard-cut "primary"))

(define-public (replace-start-forward what by)
  (replace-start what by #t))

(define-public (with-active-buffer-sub name cmd)
  (let ((old (get-name-buffer)))
    (switch-to-active-buffer name)
    (eval cmd)
    (switch-to-active-buffer old)))

(define-public-macro (with-active-buffer . l)
  (with-active-buffer-sub (car l) (cons 'begin (cdr l))))

(define-public (delayed-update nr s-cont)
  (cond ((> nr 0)
	 (system-wait "Generating automatic content" (number->string nr))
	 (generate-all-aux)
	 (update-buffer)
	 (let* ((s (number->string (- nr 1)))
		(c (escape-quotes s-cont)))
	   (exec-delayed (string-append "(delayed-update " s " \"" c "\")"))))
	(else
	 (pretend-save-buffer)
	 (exec-delayed s-cont))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For actions which need to operate on specific markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-action-path '(-1))
(define-public (set-action-path p) (set! the-action-path p))
(define-public (has-action-path?) (!= the-action-path '(-1)))
(define-public (get-action-path) the-action-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For compatibility with the old "interactive" texmacs built-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (interactive . args)
  (let ((fun (last args)))
    (if (not (procedure? fun))
        (apply tm-interactive (rcons (but-last args) (eval fun)))
        (apply tm-interactive args))))
