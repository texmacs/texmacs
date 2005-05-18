
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-dialogue.scm
;; DESCRIPTION : Interactive dialogues between Scheme and C++
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-dialogue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed execution of commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (delayed-sub body)
  (cond ((or (npair? body) (nlist? (car body)) (not (keyword? (caar body))))
	 `(lambda () ,@body #t))
	((== (caar body) :pause)
	 `(let* ((time (+ (texmacs-time) ,(cadar body)))
		 (proc ,(delayed-sub (cdr body))))
	    (lambda ()
	      (and (> (texmacs-time) time) (proc)))))
	((== (caar body) :idle)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (> (idle-time) ,(cadar body)) (proc)))))
	((== (caar body) :refresh)
	 (with sym (gensym)
	   `(begin
	      (define ,sym #f)
	      (with proc ,(delayed-sub (cdr body))
		(lambda ()
		  (and (!= ,sym (change-time))
		       (> (idle-time) ,(cadar body))
		       (proc)
		       (begin
			 (set! ,sym (change-time))
			 #f)))))))
	((== (caar body) :require)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and ,(cadar body) (proc)))))
	((== (caar body) :permanent)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (proc) (not ,(cadar body))))))
	(else (delayed-sub (cdr body)))))

(define-public-macro (delayed . body)
  `(exec-delayed ,(delayed-sub body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages and feedback on the status bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (set-temporary-message left right len)
  (set-message-temp left right #t)
  (delayed
    (:pause len)
    (recall-message)))

(define-public (texmacs-banner)
  (with tmv (string-append "GNU TeXmacs " (texmacs-version))
    (delayed
     (set-message "Welcome to GNU TeXmacs" tmv)
     (delayed
     (:pause 2500)
     (set-message "GNU TeXmacs falls under the GNU general public license" tmv)
     (delayed
     (:pause 2500)
     (set-message "GNU TeXmacs comes without any form of legal warranty" tmv)
     (delayed
     (:pause 2500)
     (set-message
      "More information about GNU TeXmacs can be found in the Help->About menu"
      tmv)
     (delayed
     (:pause 2500)
     (set-message "" ""))))))))

(define-public (interactive . args)
  (let ((fun (last args)))
    (if (not (procedure? fun))
        (apply tm-interactive (rcons (but-last args) (eval fun)))
        (apply tm-interactive args))))
