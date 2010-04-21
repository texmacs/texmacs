
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-edit.scm
;; DESCRIPTION : editing routines for versioning
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-edit)
  (:use (version version-drd)))

(tm-define (version-context? t)
  (version-tag? (tree-label t)))

(tm-define (inside-version?)
  (not (not (tree-innermost version-context?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving across the differences between both versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-first-difference)
  (go-start)
  (go-to-next-tag (group-resolve 'version-tag)))

(tm-define (version-previous-difference)
  (go-to-previous-tag (group-resolve 'version-tag)))

(tm-define (version-next-difference)
  (go-to-next-tag (group-resolve 'version-tag)))

(tm-define (version-last-difference)
  (go-end)
  (go-to-previous-tag (group-resolve 'version-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify which version to show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-show-both)
  (:context version-context?)
  (variant-replace version-context? 'version-both))

(tm-define (version-show-old)
  (:context version-context?)
  (variant-replace version-context? 'version-old))

(tm-define (version-show-new)
  (:context version-context?)
  (variant-replace version-context? 'version-new))

(tm-define (version-show-all tag)
  (tree-replace (buffer-tree) version-context? tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retaining only one version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-retain-current)
  (:context version-context?)
  (variant-replace version-context? 'version-both))

(tm-define (version-retain-old)
  (with-innermost t version-context?
    (tree-set t (tree-ref t 0))
    (version-next-difference)))

(tm-define (version-retain-new)
  (with-innermost t version-context?
    (tree-set t (tree-ref t 1))
    (version-next-difference)))

(tm-define (version-retain-current)
  (:inside version-old)
  (version-retain-old))

(tm-define (version-retain-current)
  (:inside version-new version-both)
  (version-retain-new))

(tm-define (version-retain-all which)
  (tree-replace (buffer-tree) version-context?
		(lambda (t)
		  (cond ((number? which)
			 (tree-set t (tree-ref t which)))
			((tree-is? t 'version-old)
			 (tree-set t (tree-ref t 0)))
			(else
			 (tree-set t (tree-ref t 1)))))))
