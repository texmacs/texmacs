
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

(define-public (modification-can-push? m1 m2 t)
  ;; Assuming that both m1 and m2 can be applied to the document t,
  ;; determine whether they can be applied jointly to t.
  (with i2 (modification-invert m2 t)
    (modification-can-pull? m1 i2)))

(define-public (modification-push m1 m2 t)
  ;; Assuming that both m1 and m2 can be applied to the document t,
  ;; and that both modifications can also be applied jointly,
  ;; return the modification m1* such that there exists a modification m2*
  ;; for which m2*;m1 is equivalent to m1*;m2
  (with i2 (modification-invert m2 t)
    (modification-pull m1 i2)))

(define-public (modification-co-push m1 m2 t)
  ;; Same as modification-push, but return m2* instead of m1*
  (let* ((p1 (patch-pair m1 (modification-invert m1 t)))
         (p2 (patch-pair m2 (modification-invert m2 t)))
         (r (patch-co-push p1 p2 t)))
    (patch-direct r)))

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

(define-public-macro (patch-apply! t m)
  `(set! ,t (patch-inplace-apply ,t ,m)))

(define-public (patch-can-push? p1 p2 t)
  ;; Assuming that both p1 and p2 can be applied to the document t,
  ;; determine whether they can be applied jointly to t.
  (with i2 (patch-invert p2 t)
    (patch-can-pull? p1 i2)))

(define-public (patch-push p1 p2 t)
  ;; Assuming that both p1 and p2 can be applied to the document t,
  ;; and that both patches can also be applied jointly,
  ;; return the patch p1* such that there exists a patch p2*
  ;; for which p2*;p1 is equivalent to p1*;p2
  (with i2 (patch-invert p2 t)
    (patch-pull p1 i2)))

(define-public (patch-co-push p1 p2 t)
  ;; Same as patch-push, but return p2* instead of p1*
  (let* ((i2 (patch-invert p2 t))
         (i2* (patch-co-pull p1 i2))
         (u (patch-apply t p1)))
    (patch-invert i2* u)))

(define-public (patch-equivalent? p1 p2 t)
  ;; For sanity checking
  (and (patch-applicable? p1 t)
       (patch-applicable? p2 t)
       (== (patch-apply t p1) (patch-apply t p2))))

(define-public (patch-strong-equivalent? p1 p2 t)
  ;; Strong sanity checking
  (and (patch-equivalent? p1 p2 t)
       (let* ((u (patch-apply t p1))
              (i1 (patch-invert p1 t))
              (i2 (patch-invert p2 t)))
         (and (patch-equivalent? i1 i2 u)
              (== (patch-apply u i1) t)))))
