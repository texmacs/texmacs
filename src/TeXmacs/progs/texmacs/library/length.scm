
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : length.scm
;; DESCRIPTION : subroutines for TeXmacs lengths
;; COPYRIGHT   : (C) 2001  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs library length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Length arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (length+ . ops)
  (define (length-add* ops res)
    (if (null? ops)
	res
	(length-add* (cdr ops) (length-add res (car ops)))))
  (if (null? ops)
      "0tmpt"
      (length-add* (cdr ops) (car ops))))

(tm-define (length- . ops)
  (if (null? (cdr ops))
      (string-append "-" (car ops))
      (length+ (car ops) (length- (apply length+ (cdr ops))))))

(tm-define (length-zero? len)
  (= 0 (length-decode len)))
