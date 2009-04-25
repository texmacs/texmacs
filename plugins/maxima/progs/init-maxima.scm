
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-maxima.scm
;; DESCRIPTION : Initialize maxima plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define maxima-help #f)
(define (maxima-initialize)
  (import-from (utils plugins plugin-convert))
  (import-from (utils plugins plugin-cmd))
  (import-from (dynamic session-menu))
  (import-from (maxima-kbd))
  (import-from (maxima-menus))
  (lazy-input-converter (maxima-input) maxima)
  (plugin-approx-command-set! "maxima" "float")
  (let ((help-list (string->object (var-eval-system "maxima_detect help"))))
    (if help-list
	(cond ((pair? help-list)
	       (set! maxima-help (car help-list)))
	      ((string? help-list)
	       (set! maxima-help help-list)))))
  (menu-extend session-help-icons
    (link maxima-help-icons))
  (menu-extend texmacs-extra-menu
    (if (or (in-maxima?) (and (not-in-session?) (maxima-scripts?)))
	(=> "Maxima"
	    (link maxima-menu)))))

(define (maxima-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (string-drop-right (verbatim-serialize lan t) 1)
    (cond ((== s "") "0;\n")
	  ((in? (string-ref s (- (string-length s) 1)) '(#\; #\$))
	   (string-append s "\n"))
	  (else (string-append s ";\n")))))

(define maxima-detected #f)
(define (maxima-detect)
  (when (not maxima-detected)
    (set! maxima-detected (var-eval-system "maxima_detect"))
    maxima-detected))

(define (maxima-versions)
  ;;(system "maxima_detect")
  (with version-list (string->object (maxima-detect))
    ;;(display* "Maxima versions -> " version-list "\n")
    (if (list? version-list)
	(let* ((default (car version-list))
	       (rest (cdr version-list))
	       (launch-default
		(list :launch (string-append "tm_maxima " default)))
	       (launch-rest
		(map
		 (lambda (version-name)
		   (list :launch version-name
			 (string-append "tm_maxima " version-name)))
		 rest)))
	  (cons launch-default launch-rest))
	'())))

(plugin-configure maxima
  (:require (nnot (maxima-detect)))
  (:initialize (maxima-initialize))
  ,@(maxima-versions)
  (:serializer ,maxima-serialize)
  (:session "Maxima")
  (:scripts "Maxima"))

(tm-define (script-numeric-evaluation-command)
  (:mode in-maxima?)
  "float")
