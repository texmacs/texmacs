
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtm-tidy.scm
;; DESCRIPTION : commodity routines for improving a document
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite tmtm-tidy)
  (:export tmtm-eat-space-around-control
	   tmtm-remove-superfluous-newlines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform formatting newlines into documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-modernize-newlines l)
  l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove spaces before (and possibly after) control markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-control? l)
  (and (list? l) (in? (car l) '(label index))))

(define (tmtm-eat-space-around-control-sub l)
  (cond ((< (length l) 2) l)
	((and (string? (car l))
	      (string-ends? (car l) " ")
	      (tmtm-control? (cadr l)))
	 (with r (tmtm-eat-space-around-control-sub (cddr l))
	   (if (== (car l) " ")
	       (cons (cadr l) r)
	       (cons* (string-drop-right (car l) 1) (cadr l) r))))
	(else
	 (with r (tmtm-eat-space-around-control-sub (cdr l))
	   (if (and (string? (car r))
		    (string-starts? (car r) " ")
		    (tmtm-control? (car l)))
	       (if (== (car r) " ")
		   (cons (car l) (cdr r))
		   (cons* (car l) (string-drop (car r) 1) (cdr r)))
	       (cons (car l) r))))))

(define (tmtm-eat-space-around-control l)
  (if (not (list? l)) l
      (with r (map tmtm-eat-space-around-control (cdr l))
	(if (func? l 'concat)
	    (cons 'concat (tmtm-eat-space-around-control-sub r))
	    (cons (car l) r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove superfluous newlines (i.e. remove empty paragraphs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-preserve-space? l)
  (and (list? l) (in? (car l) '(verbatim code))))

(define (tmtm-remove-superfluous-newlines l)
  (cond ((tmtm-preserve-space? l) l)
	((func? l 'document)
	 (with r (map tmtm-remove-superfluous-newlines (cdr l))
	   (cons 'document (list-filter r (lambda (x) (not (== x "")))))))
        ((pair? l)
	 (cons (car l) (map tmtm-remove-superfluous-newlines (cdr l))))
	(else l)))
