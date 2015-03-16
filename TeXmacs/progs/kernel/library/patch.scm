
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : patch.scm
;; DESCRIPTION : scheme interface for manipulating modifications and patches
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library patch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (modification kind p . args)
  (if (string? kind) (set! kind (string->symbol kind)))
  (cond ((== kind 'assign)
	 (with (t) args (modification-assign p t)))
	((== kind 'insert)
	 (with (pos t) args (modification-insert p pos t)))
	((== kind 'remove)
	 (with (pos nr) args (modification-remove p pos nr)))
	((== kind 'split)
	 (with (pos as) args (modification-split p pos as)))
	((== kind 'join)
	 (with (pos) args (modification-join p pos)))
	((== kind 'assign-node)
	 (with (lab) args (modification-assign-node p lab)))
	((== kind 'insert-node)
	 (with (pos t) args (modification-insert-node p pos t)))
	((== kind 'remove-node)
	 (with (pos) args (modification-remove-node p pos)))
	((== kind 'set-cursor)
	 (with (pos t) args (modification-set-cursor p pos t)))
	(else (texmacs-error "modification" "invalid modification type"))))

(define-public (modification-type m)
  (string->symbol (modification-kind m)))

(define-public (scheme->modification m)
  (with (k p t) m
    (make-modification (symbol->string k) p t)))

(define-public (modification->scheme m)
  (list (modification-type m)
	(modification-path m)
	(tm->stree (modification-tree m))))

(define-public-macro (modification-apply! t m)
  `(set! ,t (modification-inplace-apply ,t ,m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content patches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (patch-append . l)
  (patch-compound l))

(define-public (patch-children p)
  (map (cut patch-ref p <>) (.. 0 (patch-arity p))))

(define-public (patch->scheme p)
  (cond ((patch-pair? p)
         `(pair ,(modification->scheme (patch-direct p))
                ,(modification->scheme (patch-inverse p))))
        ((patch-compound? p)
         `(compound ,@(map patch->scheme (patch-children p))))
        ((patch-branch? p)
         `(branch ,@(map patch->scheme (patch-children p))))
        ((patch-birth? p)
         `(birth ,(patch-get-birth p) ,(patch-get-author p)))
        ((patch-author? p)
         `(author ,(patch-get-author p) ,(patch-ref p 0)))
        (else #f)))

(define-public (scheme->patch p)
  (cond ((func? p 'pair)
         (patch-pair (scheme->modification (cadr p))
                     (scheme->modification (caddr p))))
        ((func? p 'compound)
         (patch-compound (map scheme->patch (cdr p))))
        ((func? p 'branch)
         (patch-branch (map scheme->patch (cdr p))))
        ((func? p 'birth)
         (patch-birth (cadr p) (caddr p)))
        ((func? p 'author)
         (patch-birth (cadr p) (scheme->patch (caddr p))))
        (else #f)))
