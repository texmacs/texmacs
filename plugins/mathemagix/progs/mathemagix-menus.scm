
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathemagix-menus.scm
;; DESCRIPTION : Menus for the Mathemagix plugin
;; COPYRIGHT   : (C) 2012  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (mathemagix-menus)
  (:use (utils plugins plugin-cmd)
	(doc help-funcs)
	(dynamic scripts-edit)
	(convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several subroutines for the evaluation of Mathemagix expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mathemagix-apply script-apply)
(define mathemagix-insert insert)

(tm-define (script-numeric-evaluation-command)
  (:mode in-mathemagix?)
  "float")

(define (mathemagix-command cmd)
  (script->widget "Mathemagix" cmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Mathemagix packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mathemagix-used-packages (make-ahash-table))

(define (mathemagix-package-config-prefix name)
  (let ((cmd (string-concatenate (list name "-config")))
        (cmdp (string-concatenate (list (string-concatenate
					 (list name "-config"))
					" --prefix"))))
        (if (url-exists-in-path? cmd)
            (let ((x (eval-system cmdp)))
              (substring x 0 (- (string-length x) 1)))
            "")))

(define (mathemagix-package-installed? name)
  (not (equal? "" (mathemagix-package-config-prefix name))))

(define (mathemagix-simplify-as-boolean t)
  ;(display* t "\n")
  (cond ((func? t 'document) (mathemagix-simplify-as-boolean (cadr t)))
	((func? t 'math) (mathemagix-simplify-as-boolean (cadr t)))
	((equal? t "true") #t)
	 (else #f)))

(define (mathemagix-package-used? pkg)
  (if (in-session?) #t
      (let* ((lan (get-env "prog-scripts"))
	     (ses (get-env "prog-session"))
	     (cmd (string-concatenate (list "used? \"" pkg "\"")))
	     (ret (lambda (t)
		    (ahash-set! mathemagix-used-packages pkg
				(mathemagix-simplify-as-boolean t)))))
	(silent-feed* lan ses cmd ret '())
	(ahash-ref* mathemagix-used-packages pkg #f))))

(define (mathemagix-command-use pkg)
  (let ((cmd (string-concatenate (list "use \"" pkg "\""))))
    (if (in-session?)
	(mathemagix-insert cmd)
	(begin
	  (mathemagix-command cmd)
	  (mathemagix-package-used? pkg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Mathemagix menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind mathemagix-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)
  (-> "Use package"
      (if (mathemagix-package-installed? "numerix")
	  ("numerix" (mathemagix-command-use "numerix")))
      (if (mathemagix-package-installed? "algebramix")
	  ("algebramix" (mathemagix-command-use "algebramix")))
      (if (mathemagix-package-installed? "analyziz")
	  ("analyziz" (mathemagix-command-use "analyziz")))
      (if (mathemagix-package-installed? "multimix")
	  ("multimix" (mathemagix-command-use "multimix")))
      (if (mathemagix-package-installed? "graphix")
	  ("graphix" (mathemagix-command-use "graphix")))
      (if (mathemagix-package-installed? "symbolix")
	  ("symbolix" (mathemagix-command-use "symbolix"))))
  ---
  (if (mathemagix-package-used? "numerix")
      (-> "numerix"
  	  ("divide" (mathemagix-insert "infix div"))
  	  ("quotient" (mathemagix-insert "infix quo"))
  	  ("remainder" (mathemagix-insert "infix rem"))
  	  ("gcd" (mathemagix-insert "gcd"))
  	  ("lcm" (mathemagix-insert "lcm"))))
  (if (mathemagix-package-used? "algebramix")
      (-> "algebramix"
  	  ("polynomial" (mathemagix-insert "polynomial"))
  	  ("series" (mathemagix-insert "series"))
  	  ("matrix" (mathemagix-insert "matrix"))
  	  ("determinant" (mathemagix-insert "det"))
  	  ("row echelon" (mathemagix-insert "row_echelon"))
  	  ("column echelon" (mathemagix-insert "column_echelon"))
  	  ("rank" (mathemagix-insert "rank"))
  	  ("transpose" (mathemagix-insert "transpose"))))
  (if (mathemagix-package-used? "analyziz")
      (-> "analyziz"
  	  ("exponential" (mathemagix-insert "exp"))
  	  ("logarithm" (mathemagix-insert "log"))
  	  ("square root" (mathemagix-insert "sqrt"))
  	  ("cosine" (mathemagix-insert "cos"))
  	  ("sine" (mathemagix-insert "sin"))
  	  ("tangent" (mathemagix-insert "tan"))
  	  ("arc cosine" (mathemagix-insert "acos"))
  	  ("arc sine" (mathemagix-insert "asin"))
  	  ("arc tangent" (mathemagix-insert "atan"))))
  (if (mathemagix-package-used? "multimix")
      (-> "multimix"
  	  ("coordinate" (mathemagix-insert "coordinate"))
  	  ("multivarite polynomial" (mathemagix-insert "mvpolynomial"))))
  (if (mathemagix-package-used? "graphix")
      (-> "graphix"
	  ("point" (mathemagix-insert "$point"))
	  ("line" (mathemagix-insert "$line"))
  	  ("function graph" (mathemagix-insert "$graph"))))
  (if (mathemagix-package-used? "symbolix")
      (-> "symbolix"
	  ("derive" (mathemagix-insert "derive"))
	  ("replace" (mathemagix-insert "replace"))))
  (if (not-in-session?)
      ---
      (link scripts-eval-toggle-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind mathemagix-help-icons
  (if (and (in-mathemagix?) mathemagix-help)
      /
      ((balloon (icon "tm_help.xpm") "Mathemagix manual")
       (load-help-buffer mathemagix-help))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind session-help-icons
  (link mathemagix-help-icons))

(menu-bind texmacs-extra-menu
  (if (or (in-mathemagix?) (and (not-in-session?)
				(mathemagix-scripts?)))
      (=> "Mathemagix"
	  (link mathemagix-menu))))
