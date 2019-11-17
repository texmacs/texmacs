
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

(define (maxima-serialize lan t)
  (with s (string-drop-right (verbatim-serialize lan t) 1)
    (cond ((== s "") "0;\n")
	  ((in? (string-ref s (- (string-length s) 1)) '(#\; #\$))
	   (string-append s "\n"))
	  (else (string-append s ";\n")))))

(define (maxima-versions)
  (map (lambda (x)
         (string-replace (string-replace x ", lisp" "") "version " ""))
   (filter (lambda (x) (string-starts? x "version "))
     (string-split (var-eval-system "maxima --list-avail") #\nl))))

(define (maxima-launchers) ;; returns list of launchers for each version
  (if (os-mingw?)
      (if (url-exists? "$TEXMACS_HOME_PATH\\plugins\\maxima")
          `((:launch
            ,(string-append "maxima.bat -p \"" (getenv "TEXMACS_HOME_PATH")
                            "\\plugins\\maxima\\lisp\\texmacs-maxima.lisp\"")))
          `((:launch
            ,(string-append "maxima.bat -p \"" (getenv "TEXMACS_PATH")
                            "\\plugins\\maxima\\lisp\\texmacs-maxima.lisp\""))))
      (with version-list
          (if reconfigure-flag? (maxima-versions) (plugin-versions "maxima"))
        (if (and version-list (list? version-list) (nnull? version-list))
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
            '()))))

(plugin-add-macos-path "Maxima*" "Contents/Resources/maxima/bin" #f)
(plugin-add-windows-path "Maxima*" "bin" #f)

(plugin-configure maxima
  (:require (url-exists-in-path? "maxima"))
  (:versions (maxima-versions))
  ,@(maxima-launchers)
  (:serializer ,maxima-serialize)
  (:session "Maxima")
  (:scripts "Maxima"))

(when (supports-maxima?)
  (import-from (maxima-kbd))
  (import-from (maxima-menus))
  (lazy-input-converter (maxima-input) maxima)
  (plugin-approx-command-set! "maxima" "float")

  (kbd-map
    (:mode in-maxima?)
    (:mode in-math?)
    ("$" "$")))
