
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
	       (set! maxima-help help-list))))))

(define (maxima-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (string-drop-right (verbatim-serialize lan t) 1)
    (cond ((== s "") "0;\n")
	  ((in? (string-ref s (- (string-length s) 1)) '(#\; #\$))
	   (string-append s "\n"))
	  (else (string-append s ";\n")))))

(define version-list "f") ; updated by maxima-detect
(define (maxima-detect)   ; always returns boolean
  (if(os-mingw?)
     (url-exists-in-path? "maxima")
     (let ((maxima-detected #f)) (when (not maxima-detected)
       (set!  version-list (string->object(var-eval-system "maxima_detect")))
       (set! maxima-detected (list? version-list)))
       maxima-detected)))
      
(define (maxima-versions)  ; returns list of versions if any
  (if (maxima-detect) 
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
	'()))

; builds the windows command string
(define win-start-maxima (if(os-mingw?) (string-append "maxima.bat -p " (getenv "TEXMACS_PATH")  "\\plugins\\maxima\\lisp\\texmacs-maxima.lisp")))

; build the launch string according the  os version
(define (lm) (if(os-mingw?) `((:launch ,win-start-maxima)) (maxima-versions)))

(plugin-configure maxima
  (:winpath ,(url-append (url-wildcard "Maxima*") "bin"))
  (:require (maxima-detect))
  ,@(lm)
  (:initialize (maxima-initialize))
  (:serializer ,maxima-serialize)
  (:session "Maxima")
  (:scripts "Maxima"))

(kbd-map
  (:mode in-maxima?)
  (:mode in-math?)
  ("$" "$"))

(tm-define (script-numeric-evaluation-command)
  (:mode in-maxima?)
  "float")
