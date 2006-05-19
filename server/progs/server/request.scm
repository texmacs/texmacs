
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : request.scm
;; DESCRIPTION : declaration of server requests
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server request))
(use-modules (tools base) (tools abbrevs) (tools ahash-table))

(define-public request-table (make-ahash-table))

(define-public-macro (request-handler head . body)
  (let* ((s (car head))
	 (r `(lambda ,(cdr head) ,@body)))
    `(begin
       (ahash-set! request-table ',s ,r)
       (define-public ,head ,@body))))

(define-public (handle-request sock cmd)
  (and (pair? cmd)
       (with r (ahash-ref request-table (car cmd))
	 (and r (apply r (cdr cmd))))))
