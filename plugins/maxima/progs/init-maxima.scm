
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-maxima.scm
;; DESCRIPTION : Initialize maxima plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maxima-initialize)
  (import-from (texmacs texmacs tm-help) (texmacs plugin plugin-convert))
  (lazy-input-converter (maxima-input) maxima)
  (menu-extend texmacs-session-help-icons
    (if (and (in-maxima?)
	     (url-exists? "$TM_MAXIMA_HOME/info/maxima_toc.html"))
	|
	((balloon (icon "tm_help.xpm") "Maxima manual")
	 (load-help-buffer "$TM_MAXIMA_HOME/info/maxima_toc.html")))
    (if (and (in-maxima?)
	     (url-exists? "$TM_MAXIMA_HOME/doc/html/maxima_toc.html"))
	|
	((balloon (icon "tm_help.xpm") "Maxima manual")
	 (load-help-buffer "$TM_MAXIMA_HOME/doc/html/maxima_toc.html")))))

(define (maxima-serialize lan t)
  (import-from (texmacs plugin plugin-cmd))
  (with s (string-drop-right (verbatim-serialize lan t) 1)
    (cond ((== s "") "0;\n")
	  ((in? (string-ref s (- (string-length s) 1)) '(#\; #\$))
	   (string-append s "\n"))
	  (else (string-append s ";\n")))))

(define (maxima-versions)
  (let ((version-list (string->object (var-eval-system "maxima_detect"))))
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
  (:require (url-exists-in-path? "maxima"))
  (:initialize (maxima-initialize))
  ,@(maxima-versions)
  (:serializer ,maxima-serialize)
  (:session "Maxima"))
