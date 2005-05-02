
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmweb.scm
;; DESCRIPTION : automatic generation of web sites
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert doc tmweb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building a web site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmweb-convert-file tm-file html-file)
  (with-aux tm-file
    (system-mkdir (url-append html-file (url-parent)))
    (texmacs-save-buffer html-file "html")))

(define (tmweb-convert-file-dir file tm-dir html-dir)
  (let* ((u1 (url-delta (url-append tm-dir "dummy") file))
	 (u2 (url-glue (url-unglue u1 2) "html"))
	 (u3 (url-append html-dir u2)))
    (system-wait "Converting" (url->string u1))
    (display* "TeXmacs] Converting " (url->string u1) "\n")
    (tmweb-convert-file file u3)))

(define (url->list u)
  (cond ((url-none? u) '())
	((url-or? u)
	 (append (url->list (url-ref u 1))
		 (url->list (url-ref u 2))))
	(else (list u))))

(tm-define (tmweb-convert-dir tm-dir html-dir)
  (let* ((u1 (url-append tm-dir (url-any)))
	 (u2 (url-expand (url-complete u1 "dr")))
	 (u3 (url-append u2 (url-wildcard "*.tm")))
	 (u4 (url-expand (url-complete u3 "fr"))))
    (for-each (lambda (x) (tmweb-convert-file-dir x tm-dir html-dir))
	      (url->list u4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmweb-build-src (url-none))

(tm-define (tmweb-build-from u)
  (set! tmweb-build-src u)
  (exec-delayed
   "(choose-file \"Destination directory\" \"directory\" 'tmweb-build)"))

(tm-define (tmweb-build u)
  (tmweb-convert-dir tmweb-build-src u))
