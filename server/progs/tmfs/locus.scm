
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : locus.scm
;; DESCRIPTION : loci management
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tmfs locus))
(use-modules (tools base) (tools abbrevs) (tools ahash-table)
	     (server request))

(define locus-serial 0)
(define locus-table (make-ahash-table))
(define locus-inverse-table (make-ahash-table))

(request-handler (locus-new t)
  (set! locus-serial (+ locus-serial 1))
  (ahash-set! locus-table locus-serial t)
  (ahash-set! locus-inverse-table t locus-serial)
  locus-serial)

(request-handler (locus-set! locus t)
  (ahash-remove! locus-inverse-table (ahash-ref locus-table locus))
  (ahash-set! locus-table locus t)
  (ahash-set! locus-inverse-table t locus)
  #t)

(request-handler (locus-ref locus)
  (ahash-ref locus-table locus))

(define-public (locus-inverse-ref t)
  (ahash-ref locus-inverse-table t))
