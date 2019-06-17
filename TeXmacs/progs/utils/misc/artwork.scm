
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : artwork.scm
;; DESCRIPTION : automatically obtain artwork from the web or a local thumbnail
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc artwork))

;;(define up-to-date-artwork? #t)
;;(define (delayed-update-artwork)
;;  (when up-to-date-artwork?
;;    (set! up-to-date-artwork? #f)
;;    (delayed
;;      (:idle 10000)
;;      (set! up-to-date-artwork? #t)
;;      (picture-cache-reset)
;;      (update-all-buffers))))

(tmfs-format-handler (artwork name)
  (with u (tmfs-string->url (tmfs-cdr name))
    (url-format u)))

(tmfs-load-handler (artwork name)
  (let* ((u (tmfs-cdr name))
         (transform (tmfs-car name))
	 (s (string-append "thumbnail-" (url->string (url-tail u))))
	 (t (url-append (url-head u) s))
	 (thumbnail (url-append "$TEXMACS_PATH/misc" t))
	 (src (url-append "http://www.texmacs.org/artwork" u))
	 (dest (url-append "$TEXMACS_HOME_PATH/misc" u)))
    (if (url-exists? dest)
	(string-load dest)
	(with tmp (url-concretize* src)
	  (cond ((not (url-none? tmp))
                 (system-mkdir (url-head dest))
		 (system-move tmp dest)
		 (string-load dest))
		((url-exists? thumbnail)
		 (string-load thumbnail))
		(else ""))))))
