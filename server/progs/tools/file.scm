
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : file.scm
;; DESCRIPTION : basic routines on files
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tools file))
(use-modules (ice-9 rdelim) (tools base) (tools abbrevs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (ignore-errors type body)
  `(catch ,type
	  (lambda () ,body)
	  (lambda args #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (server-dir)
  (let* ((user (cuserid))
	 (info (getpw user))
	 (home (passwd:dir info)))
    (string-append home "/.texmacsd")))

(define-public (system-dir)
  (string-append (server-dir) "/system"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (save-string file s)
  (display s (open-file file OPEN_WRITE))
  (flush-all-ports)
  #t)

(define-public (load-string file)
  (read-delimited "" (open-file file OPEN_READ)))

(define-public (save-object file value)
  (write value (open-file file OPEN_WRITE))
  (flush-all-ports))

(define-public (load-object file)
  (read (open-file file OPEN_READ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (with-temp-file name s . body)
  `(with ,name (string-append (server-dir) "/system/tmpXXXXXX")
     (with p (mkstemp! ,name)
       (display ,s p)
       (flush-all-ports)
       (close-port p)
       (with r (begin ,@body)
	 (delete-file ,name)
	 r))))

(define-public (eval-system cmd)
  (let* ((temp-file (string-append (server-dir) "/system/tmpXXXXXX"))
	 (p (mkstemp! temp-file))
	 (d1 (close-port p))
	 (status (system (string-append cmd " > " temp-file)))
	 (r (if (== status 0) (load-string temp-file) #f))
	 (d2 (delete-file temp-file)))
    r))

(define-public (system* . args)
  (system (apply string-append args)))

(define-public (eval-system* . args)
  (eval-system (apply string-append args)))
